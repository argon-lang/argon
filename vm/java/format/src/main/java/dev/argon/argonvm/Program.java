package dev.argon.argonvm;

import dev.argon.argonvm.format.Argonvm;
import dev.argon.argonvm.format.VMFormatException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public abstract class Program {
	public abstract List<VMType> globalVariableTypes();

	public abstract @NotNull VMFunction getFunction(long index) throws VMFormatException;

	public abstract @NotNull VMClass getClass(long index) throws VMFormatException;

	public abstract long getClassIndex(@NotNull VMClass classInstance);

	public static @NotNull ProgramWithEntrypoint load(@NotNull Argonvm.Program program, @NotNull NativeFunctions nativeFunctions) throws VMFormatException {
		var entrypoint = loadChunk(program.getEntrypoint());

		var loadedProgram = new ProgramImpl(program, nativeFunctions);

		return new ProgramWithEntrypoint(loadedProgram, entrypoint);
	}

	private static class ProgramImpl extends Program {
		public ProgramImpl(@NotNull Argonvm.Program program, @NotNull NativeFunctions nativeFunctions) {
			this.program = program;
			functions = new VMFunction[program.getFunctionsCount()];
			classes = new VMClass[program.getClassesCount()];
			this.nativeFunctions = nativeFunctions;
		}

		private final @NotNull Argonvm.Program program;
		private final @NotNull NativeFunctions nativeFunctions;
		private final @Nullable VMFunction @NotNull[] functions;
		private final @Nullable VMClass @NotNull[] classes;

		@Override
		public List<VMType> globalVariableTypes() {
			return program.getGlobalTypesList()
				.stream()
				.map(Program::loadVMType)
				.collect(Collectors.toList());
		}

		@Override
		public @NotNull VMFunction getFunction(long index) throws VMFormatException {
			int index2 = (int)index;
			VMFunction function = functions[index2];
			if(function == null) {
				synchronized(functions) {
					function = loadFunction(program.getFunctions(index2), nativeFunctions);
					functions[index2] = function;
				}
			}
			return function;
		}

		public @NotNull VMClass getClass(long index) throws VMFormatException {
			int index2 = (int)index;
			VMClass cls = classes[index2];
			if(cls == null) {
				synchronized(classes) {
					cls = loadClass(program.getClasses(index2));
					classes[index2] = cls;
				}
			}
			return cls;
		}

		@Override
		public long getClassIndex(@NotNull VMClass classInstance) {
			for(int i = 0; i < classes.length; ++i) {
				if(classes[i] != null && classes[i].equals(classInstance)) {
					return i;
				}
			}
			throw new NoSuchElementException();
		}

		private @NotNull VMClass loadClass(@NotNull Argonvm.Class cls) throws VMFormatException {
			VMClass baseClass;
			if(cls.hasBaseClassId()) {
				baseClass = getClass(cls.getBaseClassId());
			}
			else {
				baseClass = null;
			}

			var fields = Collections.unmodifiableList(
				cls.getFieldsList()
					.stream()
					.map(field -> loadVMType(field.getType()))
					.collect(Collectors.toList())
			);

			var slots = Collections.unmodifiableList(
				cls.getSlotsList()
					.stream()
					.map(slot -> new SlotSignature(
						slot.getParameterTypesList()
							.stream()
							.map(Program::loadVMType)
							.collect(Collectors.toList()),
						loadVMType(slot.getReturnType())
					))
					.collect(Collectors.toList())
			);

			var implMap = cls.getImplementationsList()
				.stream()
				.collect(Collectors.toMap(
					impl -> new SlotKey(impl.getDeclaringClassId(), impl.getSlotIndex()),
					impl -> impl.getFunctionIndex()
				));


			return new VMClass() {
				@Override
				public VMClass baseClass() {
					return baseClass;
				}

				@Override
				public List<VMType> fields() {
					return fields;
				}

				@Override
				public List<SlotSignature> slots() {
					return slots;
				}

				@Override
				public long getImplementationFunctionId(SlotKey slotKey) {
					Long result = implMap.get(slotKey);
					if(result != null) {
						return result;
					}

					if(baseClass != null) {
						return baseClass.getImplementationFunctionId(slotKey);
					}
					else {
						throw new NoSuchElementException();
					}
				}
			};
		}
	}

	private static @NotNull VMFunction loadFunction(@NotNull Argonvm.Function function, @NotNull NativeFunctions nativeFunctions) throws VMFormatException {
		if(function.hasBytecode()) {
			var bytecode = function.getBytecode();
			var parameterTypes =
				bytecode.getParameterTypesList()
					.stream()
					.map(Program::loadVMType)
					.collect(Collectors.toList());

			var returnType = loadVMType(bytecode.getReturnType());

			var chunk = loadChunk(bytecode.getBody());

			return new BytecodeVMFunction(parameterTypes, returnType, chunk);
		}
		else if(function.hasNative()) {
			var nativeFunc = nativeFunctions.get(function.getNative());
			if(nativeFunc == null) {
				throw new NoSuchElementException("Unknown native function: " + function.getNative());
			}
			return nativeFunc;
		}
		else {
			throw new IllegalArgumentException();
		}
	}

	private static @NotNull Chunk loadChunk(@NotNull Argonvm.Chunk chunk) throws VMFormatException {
		var constants = new Object[chunk.getConstantsCount()];
		for(int i = 0; i < chunk.getConstantsCount(); ++i) {
			constants[i] = loadConstant(chunk.getConstants(i));
		}

		var variables = new VMType[chunk.getVariableTypesCount()];
		for(int i = 0; i < chunk.getVariableTypesCount(); ++i) {
			variables[i] = loadVMType(chunk.getVariableTypes(i));
		}

		var labelTargets = new int[chunk.getLabelTargetsCount()];
		for(int i = 0; i < chunk.getLabelTargetsCount(); ++i) {
			labelTargets[i] = (int)chunk.getLabelTargets(i);
		}


		return new Chunk(constants, variables, labelTargets, chunk.getBytecode());
	}

	private static @Nullable Object loadConstant(@NotNull Argonvm.ConstantValue constantValue) {
		if(constantValue.hasInt8()) {
			return Byte.valueOf((byte)constantValue.getInt8());
		}
		else if(constantValue.hasInt16()) {
			return Short.valueOf((short)constantValue.getInt16());
		}
		else if(constantValue.hasInt32()) {
			return constantValue.getInt32();
		}
		else if(constantValue.hasInt64()) {
			return constantValue.getInt64();
		}
		else if(constantValue.hasFloat32()) {
			return constantValue.getFloat32();
		}
		else if(constantValue.hasFloat64()) {
			return constantValue.getFloat64();
		}
		else if(constantValue.hasTuple()) {
			var tuple = constantValue.getTuple();
			var elements = new Object[tuple.getElementsCount()];
			for(int i = 0; i < tuple.getElementsCount(); ++i) {
				elements[i] = loadConstant(tuple.getElements(i));
			}
			return new VMTuple(elements);
		}
		else if(constantValue.hasStringLiteral()) {
			return constantValue.getStringLiteral();
		}
		else if(constantValue.hasBytesLiteral()) {
			return constantValue.getBytesLiteral().toByteArray();
		}
		else {
			throw new IllegalArgumentException();
		}
	}

	public static @NotNull VMType loadVMType(@NotNull Argonvm.ValueType t) {
		if(t.hasSimple()) {
			return switch(t.getSimple()) {
				case VALUE_TYPE_SIMPLE_INT8 -> new VMType.Int8();
				case VALUE_TYPE_SIMPLE_INT16 -> new VMType.Int16();
				case VALUE_TYPE_SIMPLE_INT32 -> new VMType.Int32();
				case VALUE_TYPE_SIMPLE_INT64 -> new VMType.Int64();
				case VALUE_TYPE_SIMPLE_FLOAT32 -> new VMType.Float32();
				case VALUE_TYPE_SIMPLE_FLOAT64 -> new VMType.Float64();
				case VALUE_TYPE_SIMPLE_OBJECT_REFERENCE -> new VMType.ObjectReference();
			};
		}
		else if(t.hasTuple()) {
			var tuple = t.getTuple();
			var elements = new VMType[tuple.getElementsCount()];
			for(int i = 0; i < tuple.getElementsCount(); ++i) {
				elements[i] = loadVMType(tuple.getElements(i));
			}
			return new VMType.Tuple(List.of(elements));
		}
		else {
			throw new IllegalArgumentException();
		}
	}
}
