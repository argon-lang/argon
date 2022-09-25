package dev.argon.argonvm;

import dev.argon.argonvm.format.Argonvm;

import java.util.*;
import java.util.stream.Collectors;

public abstract class Program {
	public abstract RTT rtt();
	public abstract VMFunction getFunction(long index) throws VMFormatException;

	public abstract VMClass getClass(long index) throws VMFormatException;

	public abstract long getClassIndex(VMClass classInstance);

	public static ProgramWithEntrypoint load(Argonvm.Program program, NativeFunctions nativeFunctions) throws VMFormatException {
		var rtt = new RTT() {};


		var entrypoint = loadChunk(program.getEntrypoint());

		var loadedProgram = new ProgramImpl(program, nativeFunctions);

		return new ProgramWithEntrypoint(loadedProgram, entrypoint);
	}

	private static class ProgramImpl extends Program {
		public ProgramImpl(Argonvm.Program program, NativeFunctions nativeFunctions) {
			this.program = program;
			functions = new VMFunction[program.getFunctionsCount()];
			classes = new VMClass[program.getClassesCount()];
			this.nativeFunctions = nativeFunctions;
		}

		private final Argonvm.Program program;
		private final NativeFunctions nativeFunctions;

		private final RTT rtt = new RTT() {};
		private final VMFunction[] functions;
		private final VMClass[] classes;

		@Override
		public RTT rtt() {
			return rtt;
		}

		@Override
		public VMFunction getFunction(long index) throws VMFormatException {
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

		public VMClass getClass(long index) throws VMFormatException {
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
		public long getClassIndex(VMClass classInstance) {
			for(int i = 0; i < classes.length; ++i) {
				if(classes[i] != null && classes[i].equals(classInstance)) {
					return i;
				}
			}
			throw new NoSuchElementException();
		}

		private VMClass loadClass(Argonvm.Class cls) throws VMFormatException {
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
					for(VMClass cls = this; cls != null; cls = cls.baseClass()) {
						Long result = implMap.get(slotKey);
						if(result != null) {
							return result;
						}
					}
					throw new NoSuchElementException();
				}
			};
		}
	}

	private static VMFunction loadFunction(Argonvm.Function function, NativeFunctions nativeFunctions) throws VMFormatException {
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
			return nativeFunctions.get(function.getNative());
		}
		else {
			throw new IllegalArgumentException();
		}
	}

	private static Chunk loadChunk(Argonvm.Chunk chunk) throws VMFormatException {
		var constants = new Object[chunk.getConstantsCount()];
		for(int i = 0; i < chunk.getConstantsCount(); ++i) {
			constants[i] = loadConstant(chunk.getConstants(i));
		}

		var variables = new VMType[chunk.getVariableTypesCount()];
		for(int i = 0; i < chunk.getVariableTypesCount(); ++i) {
			variables[i] = loadVMType(chunk.getVariableTypes(i));
		}

		return new Chunk(constants, variables, chunk.getBytecode());
	}

	private static Object loadConstant(Argonvm.ConstantValue constantValue) {
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
		else {
			throw new IllegalArgumentException();
		}
	}

	public static VMType loadVMType(Argonvm.ValueType t) {
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