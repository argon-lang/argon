package dev.argon.backend.codegen;

import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmExtern;
import dev.argon.vm.*;
import dev.argon.vm.Instruction;

import java.io.IOException;
import java.lang.classfile.Attributes;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.classfile.CodeBuilder;
import java.lang.classfile.Label;
import java.lang.classfile.TypeKind;
import java.lang.classfile.attribute.ModuleAttribute;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.lang.constant.ModuleDesc;
import java.lang.constant.PackageDesc;
import java.util.*;
import java.util.function.Consumer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

final class Emitter {
	private static final ClassFile CLASS_FILE = ClassFile.of();
	private static final String MODULE_INFO_ENTRY = "module-info.class";

	public Emitter(ProgramModel program, ZipOutputStream zos) {
		this.program = program;
		this.zos = zos;
	}

	private final ProgramModel program;
	private final ZipOutputStream zos;
	private boolean emittedModuleInfo = false;

	public void emit() throws IOException {
		emitClassFiles();
	}

	public void emitClassFiles() throws IOException {
		var requiredExports = requiredExports();

		for(var classfile : program.decodedMetadata().platformMetadata().additionalClasses().orElse(List.of())) {
			var classModel = classfile.model();
			if(classModel.isModuleInfo()) {
				writeEntry(MODULE_INFO_ENTRY, moduleInfoBytesWithExports(classModel, requiredExports));
				emittedModuleInfo = true;
			}
			else {
				writeEntry(classEntryName(classModel), classfile.toByteArray());
			}
		}

		emitModuleInfo();

		for(var module : program.modules()) {
			emitModule(module);
		}
	}

	private void emitModuleInfo() throws IOException {
		if(!emittedModuleInfo) {
			var moduleAttribute = ModuleAttribute.of(
				ModuleDesc.of(ClassNaming.currentTubeModuleName(program)),
				builder -> {
					builder.requires(ModuleDesc.of("java.base"), ClassFile.ACC_MANDATED, null);
					for(var packageName : requiredExports()) {
						builder.exports(PackageDesc.of(packageName), 0);
					}
				}
			);

			var miBytes = CLASS_FILE.buildModule(moduleAttribute);

			writeEntry(MODULE_INFO_ENTRY, miBytes);
			emittedModuleInfo = true;
		}
	}

	private Set<String> requiredExports() {
		var exports = new TreeSet<String>();

		for(var module : program.modules()) {
			exports.add(ClassNaming.currentTubeModulePackageName(program, module.path()));
		}

		program.decodedMetadata().platformMetadata().moduleMetadata()
			.ifPresent(moduleMetadata ->
				moduleMetadata.stream()
					.flatMap(metadata -> metadata.packageName().stream())
					.forEach(exports::add)
			);

		return exports;
	}

	private byte[] moduleInfoBytesWithExports(ClassModel moduleInfo, Set<String> requiredExports) {
		var moduleAttribute = moduleInfo.findAttribute(Attributes.module())
			.orElseThrow(() -> new IllegalArgumentException("module-info classfile is missing its Module attribute"));
		var augmentedModuleAttribute = moduleAttributeWithExports(moduleAttribute, requiredExports);

		var transform = ClassTransform
			.dropping(element -> element instanceof ModuleAttribute)
			.andThen(ClassTransform.endHandler(builder -> builder.with(augmentedModuleAttribute)));

		return CLASS_FILE.transformClass(moduleInfo, transform);
	}

	private ModuleAttribute moduleAttributeWithExports(ModuleAttribute moduleAttribute, Set<String> requiredExports) {
		var existingExports = new HashSet<String>();
		for(var exportInfo : moduleAttribute.exports()) {
			existingExports.add(exportInfo.exportedPackage().asSymbol().name());
		}

		return ModuleAttribute.of(
			moduleAttribute.moduleName(),
			builder -> {
				builder.moduleFlags(moduleAttribute.moduleFlagsMask());
				moduleAttribute.moduleVersion()
					.ifPresent(version -> builder.moduleVersion(version.stringValue()));

				for(var requireInfo : moduleAttribute.requires()) {
					builder.requires(requireInfo);
				}
				for(var exportInfo : moduleAttribute.exports()) {
					builder.exports(exportInfo);
				}
				for(var packageName : requiredExports) {
					if(!existingExports.contains(packageName)) {
						builder.exports(PackageDesc.of(packageName), 0);
					}
				}
				for(var openInfo : moduleAttribute.opens()) {
					builder.opens(openInfo);
				}
				for(var usedClass : moduleAttribute.uses()) {
					builder.uses(usedClass);
				}
				for(var provideInfo : moduleAttribute.provides()) {
					builder.provides(provideInfo);
				}
			}
		);
	}

	private String classEntryName(ClassModel classModel) {
		return classModel.thisClass().asInternalName() + ".class";
	}

	private void writeEntry(String name, byte[] contents) throws IOException {
		zos.putNextEntry(new ZipEntry(name));
		try {
			zos.write(contents);
		}
		finally {
			zos.closeEntry();
		}
	}

	private void emitModule(ProgramModel.ModuleModel module) throws IOException {
		var functions = new ArrayList<TubeFileEntry.FunctionDefinition>();

		for(var entry : module.exports()) {
			switch(entry) {
				case ProgramModel.ModuleExportEntry.FunctionDefinition(var func) ->
					functions.add(func);

				case ProgramModel.ModuleExportEntry.RecordDefinition(var rec) ->
					emitRecord(rec);

				case ProgramModel.ModuleExportEntry.EnumDefinition(var enumDef) ->
					emitEnum(enumDef);

				case ProgramModel.ModuleExportEntry.TraitDefinition(var traitDef) ->
					emitTrait(traitDef);

				case ProgramModel.ModuleExportEntry.InstanceDefinition(var instanceDef) ->
					emitInstance(instanceDef);
			}
		}

		if(!functions.isEmpty()) {
			emitGlobalFunctions(functions);
		}
	}

	private void emitGlobalFunctions(List<TubeFileEntry.FunctionDefinition> functions) throws IOException {
		var classDesc = ClassNaming.moduleGlobalFunctionsClassName(program, modulePathForFunctions(functions));
		var bytes = CLASS_FILE.build(classDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object);

			classBuilder.withMethodBody(
				ConstantDescs.INIT_NAME,
				ConstantDescs.MTD_void,
				ClassFile.ACC_PRIVATE,
				codeBuilder -> codeBuilder
					.aload(0)
					.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void)
					.return_()
			);

			for(var entry : functions) {
				var function = entry.definition();
				classBuilder.withMethodBody(
					ClassNaming.functionName(function._import()),
					functionMethodType(function),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
					codeBuilder -> emitGlobalFunctionBody(codeBuilder, function)
				);
			}
		});

		writeEntry(classEntryName(classDesc), bytes);
	}

	private ModulePath modulePathForFunctions(List<TubeFileEntry.FunctionDefinition> functions) {
		if(functions.isEmpty()) {
			throw new IllegalArgumentException("No functions to emit");
		}

		var firstImport = functions.getFirst().definition()._import();
		var moduleId = getModuleId(firstImport);
		return program.getModuleInfo(moduleId).path();
	}

	private void emitGlobalFunctionBody(CodeBuilder cb, FunctionDefinition function) {
		var implementation = function.implementation();
		if(implementation.isEmpty()) {
			throw new RuntimeException("Function implementation is missing");
		}

		switch(implementation.get()) {
			case FunctionImplementation.Extern extern -> emitExternFunctionBody(cb, function, extern);
			case FunctionImplementation.VmIr ir -> {
				int[] tokenSlots = new int[function.signature().tokenParameters().size()];
				int[] registerSlots = new int[Math.addExact(function.signature().parameters().size(), ir.body().variables().variables().size())];
				TypeKind[] registerKinds = new TypeKind[registerSlots.length];

				for(int i = 0; i < function.signature().tokenParameters().size(); ++i) {
					tokenSlots[i] = cb.parameterSlot(i);
				}

				for(int i = 0; i < function.signature().parameters().size(); ++i) {
					var parameterType = tokenAsClassDesc(function.signature().parameters().get(i).paramType());
					registerSlots[i] = cb.parameterSlot(function.signature().tokenParameters().size() + i);
					registerKinds[i] = TypeKind.from(parameterType).asLoadable();
				}

				for(int i = 0; i < ir.body().variables().variables().size(); ++i) {
					var variable = ir.body().variables().variables().get(i);
					var varType = tokenAsClassDesc(variable.type());
					var varKind = TypeKind.from(varType).asLoadable();
					registerSlots[function.signature().parameters().size() + i] = cb.allocateLocal(varKind);
					registerKinds[function.signature().parameters().size() + i] = varKind;
				}

				var blockEmitter = new BlockEmitter(cb, tokenSlots, registerSlots, registerKinds);
				blockEmitter.emitBlock(ir.body().block());
			}
		}
	}

	private void emitExternFunctionBody(
		CodeBuilder cb,
		FunctionDefinition function,
		FunctionImplementation.Extern extern
	) {
		var jvmFunction = decodeJvmFunction(extern);
		var externMethod = resolveExternMethod(jvmFunction);
		var externMethodType = MethodTypeDesc.ofDescriptor(jvmFunction.descriptor());

		var localSlot = 0;
		for(var parameterType : functionParameterTypes(function)) {
			var parameterKind = TypeKind.from(parameterType).asLoadable();
			cb.loadLocal(parameterKind, localSlot);
			localSlot += parameterKind.slotSize();
		}

		cb
			.invokestatic(
				ClassDesc.ofInternalName(jvmFunction._class()),
				externMethod,
				externMethodType
			)
			.areturn();
	}

	private JvmExtern.JvmFunction decodeJvmFunction(FunctionImplementation.Extern extern) {
		try {
			return (JvmExtern.JvmFunction)JvmExtern.codec().decode(extern.extern());
		}
		catch(DecodeException ex) {
			throw new IllegalArgumentException("Invalid JVM extern function metadata", ex);
		}
	}

	private String resolveExternMethod(JvmExtern.JvmFunction extern) {
		String methodName = null;

		for(var classfile : program.decodedMetadata().platformMetadata().additionalClasses().orElse(List.of())) {
			var classModel = classfile.model();
			if(!classModel.thisClass().asInternalName().equals(extern._class())) {
				continue;
			}

			for(var method : classModel.methods()) {
				if(!method.methodType().stringValue().equals(extern.descriptor())) {
					continue;
				}

				if(methodName != null) {
					throw new IllegalArgumentException("Ambiguous JVM extern function method: " + extern);
				}

				methodName = method.methodName().stringValue();
			}
		}

		if(methodName == null) {
			throw new IllegalArgumentException("Could not find JVM extern function method: " + extern);
		}

		return methodName;
	}

	private MethodTypeDesc functionMethodType(FunctionDefinition function) {
		return MethodTypeDesc.of(
			ClassDesc.of("dev.argon.runtime.Trampoline"),
			functionParameterTypes(function)
		);
	}

	private List<ClassDesc> functionParameterTypes(FunctionDefinition function) {
		var parameterTypes = new ArrayList<ClassDesc>();

		for(int i = 0; i < function.signature().tokenParameters().size(); ++i) {
			parameterTypes.add(ClassDesc.of("dev.argon.runtime.Token"));
		}
		for(var parameter : function.signature().parameters()) {
			parameterTypes.add(tokenAsClassDesc(parameter.paramType()));
		}

		return parameterTypes;
	}

	private UnsignedBigInteger getModuleId(dev.argon.vm.ImportSpecifier importSpecifier) {
		return switch(importSpecifier) {
			case dev.argon.vm.ImportSpecifier.Global global -> global.moduleId();
			case dev.argon.vm.ImportSpecifier.Local local -> getModuleId(local.parent());
		};
	}

	private void emitRecord(TubeFileEntry.RecordDefinition rec) throws IOException {
		var definition = rec.definition();
		var classDesc = ClassNaming.typeDefinitionClassDescriptor(program, definition._import());
		var fields = definition.fields();
		var fieldDescs = fields.stream()
			.map(field -> tokenAsClassDesc(field.fieldType()))
			.toList();

		var bytes = CLASS_FILE.build(classDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object);

			for(int i = 0; i < fields.size(); ++i) {
				var field = fields.get(i);
				var flags = ClassFile.ACC_PUBLIC;
				if(!field.mutable()) {
					flags |= ClassFile.ACC_FINAL;
				}

				classBuilder.withField(
					ClassNaming.fieldName(field.name()),
					fieldDescs.get(i),
					flags
				);
			}

			classBuilder.withMethodBody(
				ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, fieldDescs),
				ClassFile.ACC_PUBLIC,
				codeBuilder -> {
					codeBuilder
						.aload(0)
						.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < fields.size(); ++i) {
						var fieldDesc = fieldDescs.get(i);
						var fieldKind = TypeKind.from(fieldDesc).asLoadable();
						codeBuilder
							.aload(0)
							.loadLocal(fieldKind, localSlot)
							.putfield(classDesc, ClassNaming.fieldName(fields.get(i).name()), fieldDesc);
						localSlot += fieldKind.slotSize();
					}

					codeBuilder.return_();
				}
			);
		});

		writeEntry(classEntryName(classDesc), bytes);
	}

	private void emitEnum(TubeFileEntry.EnumDefinition enumDef) {
		throw new RuntimeException("Enum definition not implemented");
	}

	private void emitTrait(TubeFileEntry.TraitDefinition traitDef) {
		throw new RuntimeException("Trait definition not implemented");
	}

	private void emitInstance(TubeFileEntry.InstanceDefinition instanceDef) {
		throw new RuntimeException("Instance definition not implemented");
	}

	private ClassDesc tokenAsClassDesc(Token token) {
		return switch(token) {
			case Token.Boxed _ -> ConstantDescs.CD_Object;
			case Token.Builtin(var bt) -> switch(bt) {
				case BuiltinType.Array(var elementType) -> tokenAsClassDesc(elementType).arrayType();
				case BuiltinType.Bool() -> ConstantDescs.CD_boolean;
				case BuiltinType.Conjunction _ -> throw new RuntimeException("Conjunction not implemented");
				case BuiltinType.Disjunction _ -> throw new RuntimeException("Disjunction not implemented");
				case BuiltinType.Int() -> ClassDesc.of("java.math.BigInteger");
				case BuiltinType.Never() -> ClassDesc.of("dev.argon.runtime.Never");
				case BuiltinType.String() -> ConstantDescs.CD_String;
			};
			case Token.Enum enumToken -> {
				var enumInfo = program.getEnumInfo(enumToken.enumId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, enumInfo.importSpecifier());
			}
			case Token.Function _ -> ClassDesc.of("dev.argon.runtime.Function");
			case Token.FunctionErased _ -> ClassDesc.of("dev.argon.runtime.FunctionErased");
			case Token.FunctionToken _ -> ClassDesc.of("dev.argon.runtime.FunctionToken");
			case Token.InstanceType instanceType -> {
				var instanceInfo = program.getInstanceInfo(instanceType.instanceId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, instanceInfo.importSpecifier());
			}
			case Token.InstanceValue _ -> {
				throw new UnsupportedOperationException("InstanceValue cannot be used as a type");
			}
			case Token.ParentTokenParameter _ -> ConstantDescs.CD_Object;
			case Token.Record record -> {
				var recordInfo = program.getRecordInfo(record.recordId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, recordInfo.importSpecifier());
			}
			case Token.RefCell _ -> ClassDesc.of("dev.argon.runtime.RefCell");
			case Token.TokenParameter _ -> ConstantDescs.CD_Object;
			case Token.Trait trait -> {
				var traitInfo = program.getTraitInfo(trait.traitId());
				yield ClassNaming.typeDefinitionClassDescriptor(program, traitInfo.importSpecifier());
			}
			case Token.Tuple tuple -> {
				if(tuple.elements().size() > 10) {
					yield ClassDesc.of("dev.argon.runtime.TupleXL");
				}
				else {
					yield ClassDesc.of("dev.argon.runtime.Tuple" + tuple.elements().size());
				}
			}
			case Token.TypeInfo() -> ClassDesc.of("dev.argon.runtime.TypeInfo");
		};
	}

	private String classEntryName(ClassDesc classDesc) {
		var descriptor = classDesc.descriptorString();
		if(!descriptor.startsWith("L") || !descriptor.endsWith(";")) {
			throw new IllegalArgumentException("Class descriptor is not a class type: " + descriptor);
		}

		return descriptor.substring(1, descriptor.length() - 1) + ".class";
	}

	private final class BlockEmitter {
		private static final ClassDesc CD_BIG_INTEGER = ClassDesc.of("java.math.BigInteger");
		private static final ClassDesc CD_TRAMPOLINE_RESULT = ClassDesc.of("dev.argon.runtime", "Trampoline$Result");
		private static final ClassDesc CD_UNSUPPORTED_OPERATION_EXCEPTION =
			ClassDesc.of("java.lang.UnsupportedOperationException");

		public BlockEmitter(
			CodeBuilder cb,
			int[] tokenSlots,
			int[] registerSlots,
			TypeKind[] registerKinds
		) {
			this.cb = cb;
			this.tokenSlots = tokenSlots;
			this.registerSlots = registerSlots;
			this.registerKinds = registerKinds;
		}

		private final CodeBuilder cb;
		private final int[] tokenSlots;
		private final int[] registerSlots;
		private final TypeKind[] registerKinds;
		private final Map<BlockId, BlockLabels> blocks = new HashMap<>();

		public void emitBlock(Block block) {
			for(var instruction : block.instructions()) {
				emitInstruction(instruction);
			}
		}

		private void emitInstruction(Instruction instruction) {
			switch(instruction) {
				case Instruction.ConstString constString -> {
					cb.ldc(constString.value());
					storeRegister(constString.dest());
				}

				case Instruction.ConstInt constInt -> {
					cb
						.new_(CD_BIG_INTEGER)
						.dup()
						.ldc(constInt.value().toString())
						.invokespecial(
							CD_BIG_INTEGER,
							ConstantDescs.INIT_NAME,
							MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String)
						);
					storeRegister(constInt.dest());
				}

				case Instruction.ConstBool constBool -> {
					if(constBool.value()) {
						cb.iconst_1();
					}
					else {
						cb.iconst_0();
					}
					storeRegister(constBool.dest());
				}

				case Instruction.Move move -> {
					loadRegister(move.src());
					storeRegister(move.dest());
				}

				case Instruction.Return ret -> {
					cb
						.new_(CD_TRAMPOLINE_RESULT)
						.dup();
					loadRegister(ret.src());
					boxValue(registerKind(ret.src()));
					cb
						.invokespecial(
							CD_TRAMPOLINE_RESULT,
							ConstantDescs.INIT_NAME,
							MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object)
						)
						.areturn();
				}

				case Instruction.Unreachable ignored ->
					emitRuntimeUnsupported("Unreachable instruction executed");

				case Instruction.Block blockInsn -> {
					var start = cb.newLabel();
					var end = cb.newLabel();
					blocks.put(blockInsn.blockId(), new BlockLabels(start, end));

					cb.labelBinding(start);
					emitBlock(blockInsn.body());
					if(blockInsn.flags().isLoop()) {
						cb.goto_(start);
					}
					cb.labelBinding(end);
				}

				case Instruction.BlockBreak breakInsn -> {
					cb.goto_(blocks.get(breakInsn.blockId()).end);
				}

				case Instruction.BlockBreakIf breakIf -> {
					cb.iload(registerSlot(breakIf.condition()));
					cb.ifne(blocks.get(breakIf.blockId()).end);
				}

				case Instruction.BlockBreakUnless breakUnless -> {
					cb.iload(registerSlot(breakUnless.condition()));
					cb.ifeq(blocks.get(breakUnless.blockId()).end);
				}

				case Instruction.BlockRetry retry -> {
					cb.goto_(blocks.get(retry.blockId()).start);
				}

				case Instruction.Box box -> {
					loadRegister(box.value());
					boxValue(registerKind(box.value()));
					storeRegister(box.dest());
				}
				
				case Instruction.Builtin builtin -> emitBuiltin(builtin.op());

				case Instruction.EnumVariantLiteral ignored -> unsupportedInstruction(instruction);

				case Instruction.Finally ignored -> unsupportedInstruction(instruction);
				case Instruction.FunctionCall ignored -> unsupportedInstruction(instruction);
				case Instruction.FunctionObjectCall ignored -> unsupportedInstruction(instruction);
				case Instruction.FunctionObjectErasedCall ignored -> unsupportedInstruction(instruction);
				case Instruction.FunctionObjectTokenCall ignored -> unsupportedInstruction(instruction);
				case Instruction.IfElse ignored -> unsupportedInstruction(instruction);
				case Instruction.InstanceMethodCall ignored -> unsupportedInstruction(instruction);
				case Instruction.IsEnumVariantOrBreak ignored -> unsupportedInstruction(instruction);
				case Instruction.LoadInstanceField ignored -> unsupportedInstruction(instruction);
				case Instruction.LoadReference ignored -> unsupportedInstruction(instruction);
				case Instruction.LoadToken ignored -> unsupportedInstruction(instruction);
				case Instruction.NewInstance ignored -> unsupportedInstruction(instruction);
				case Instruction.NewReference ignored -> unsupportedInstruction(instruction);
				case Instruction.PartiallyAppliedFunction ignored -> unsupportedInstruction(instruction);
				case Instruction.PartiallyAppliedFunctionErased ignored -> unsupportedInstruction(instruction);
				case Instruction.PartiallyAppliedTokenFunction ignored -> unsupportedInstruction(instruction);
				case Instruction.Raise ignored -> unsupportedInstruction(instruction);
				case Instruction.RecordFieldLoad ignored -> unsupportedInstruction(instruction);
				case Instruction.RecordFieldStore ignored -> unsupportedInstruction(instruction);
				case Instruction.RecordLiteral ignored -> unsupportedInstruction(instruction);
				case Instruction.Tuple ignored -> unsupportedInstruction(instruction);
				case Instruction.TupleElement ignored -> unsupportedInstruction(instruction);
				case Instruction.Unbox ignored -> unsupportedInstruction(instruction);
				case Instruction.UpdateReference ignored -> unsupportedInstruction(instruction);
			}
		}

		private void loadRegister(RegisterId register) {
			cb.loadLocal(registerKind(register), registerSlot(register));
		}

		private void storeRegister(RegisterId register) {
			cb.storeLocal(registerKind(register), registerSlot(register));
		}

		private int registerSlot(RegisterId register) {
			return registerSlots[register.id().toBigInteger().intValueExact()];
		}

		private TypeKind registerKind(RegisterId register) {
			return registerKinds[register.id().toBigInteger().intValueExact()];
		}

		private void emitBuiltin(BuiltinOp op) {
			switch(op) {
				case BuiltinOp.ArrayCreateUnsafeUninitialized arrayCreate -> {
					var elementType = tokenAsClassDesc(arrayCreate.elementType());
					var elementKind = TypeKind.from(elementType).asLoadable();

					loadBigIntegerAsInt(arrayCreate.length());
					if(elementKind == TypeKind.REFERENCE) {
						cb.anewarray(elementType);
					}
					else {
						cb.newarray(elementKind);
					}
					storeRegister(arrayCreate.dest());
				}

				case BuiltinOp.ArrayGet arrayGet -> {
					loadRegister(arrayGet.array());
					loadBigIntegerAsInt(arrayGet.index());
					cb.arrayLoad(arrayElementKind(arrayGet.elementType()));
					storeRegister(arrayGet.dest());
				}

				case BuiltinOp.ArrayLength arrayLength -> {
					loadRegister(arrayLength.array());
					cb.arraylength();
					intToBigInteger();
					storeRegister(arrayLength.dest());
				}

				case BuiltinOp.ArraySet arraySet -> {
					loadRegister(arraySet.array());
					loadBigIntegerAsInt(arraySet.index());
					loadRegister(arraySet.value());
					cb.arrayStore(arrayElementKind(arraySet.elementType()));
				}

				case BuiltinOp.BoolEq boolEq -> {
					loadRegister(boolEq.lhs());
					loadRegister(boolEq.rhs());
					emitBooleanFromBranch(cb::if_icmpeq);
					storeRegister(boolEq.dest());
				}

				case BuiltinOp.BoolNot boolNot -> {
					loadRegister(boolNot.value());
					cb.iconst_1().ixor();
					storeRegister(boolNot.dest());
				}

				case BuiltinOp.IntAdd intAdd ->
					emitBigIntegerBinary(intAdd.dest(), intAdd.lhs(), intAdd.rhs(), "add");

				case BuiltinOp.IntBitAnd intBitAnd ->
					emitBigIntegerBinary(intBitAnd.dest(), intBitAnd.lhs(), intBitAnd.rhs(), "and");

				case BuiltinOp.IntBitNot intBitNot ->
					emitBigIntegerUnary(intBitNot.dest(), intBitNot.value(), "not");

				case BuiltinOp.IntBitOr intBitOr ->
					emitBigIntegerBinary(intBitOr.dest(), intBitOr.lhs(), intBitOr.rhs(), "or");

				case BuiltinOp.IntBitShiftLeft intShiftLeft ->
					emitBigIntegerShift(intShiftLeft.dest(), intShiftLeft.lhs(), intShiftLeft.rhs(), "shiftLeft");

				case BuiltinOp.IntBitShiftRight intShiftRight ->
					emitBigIntegerShift(intShiftRight.dest(), intShiftRight.lhs(), intShiftRight.rhs(), "shiftRight");

				case BuiltinOp.IntBitXor intBitXor ->
					emitBigIntegerBinary(intBitXor.dest(), intBitXor.lhs(), intBitXor.rhs(), "xor");

				case BuiltinOp.IntEq intEq ->
					emitBigIntegerCompare(intEq.dest(), intEq.lhs(), intEq.rhs(), cb::ifeq);

				case BuiltinOp.IntGe intGe ->
					emitBigIntegerCompare(intGe.dest(), intGe.lhs(), intGe.rhs(), cb::ifge);

				case BuiltinOp.IntGt intGt ->
					emitBigIntegerCompare(intGt.dest(), intGt.lhs(), intGt.rhs(), cb::ifgt);

				case BuiltinOp.IntLe intLe ->
					emitBigIntegerCompare(intLe.dest(), intLe.lhs(), intLe.rhs(), cb::ifle);

				case BuiltinOp.IntLt intLt ->
					emitBigIntegerCompare(intLt.dest(), intLt.lhs(), intLt.rhs(), cb::iflt);

				case BuiltinOp.IntMul intMul ->
					emitBigIntegerBinary(intMul.dest(), intMul.lhs(), intMul.rhs(), "multiply");

				case BuiltinOp.IntNegate intNegate ->
					emitBigIntegerUnary(intNegate.dest(), intNegate.value(), "negate");

				case BuiltinOp.IntSub intSub ->
					emitBigIntegerBinary(intSub.dest(), intSub.lhs(), intSub.rhs(), "subtract");

				case BuiltinOp.StringConcat stringConcat -> {
					loadRegister(stringConcat.lhs());
					loadRegister(stringConcat.rhs());
					cb.invokevirtual(
						ConstantDescs.CD_String,
						"concat",
						MethodTypeDesc.of(ConstantDescs.CD_String, ConstantDescs.CD_String)
					);
					storeRegister(stringConcat.dest());
				}

				case BuiltinOp.StringEq stringEq -> {
					loadRegister(stringEq.lhs());
					loadRegister(stringEq.rhs());
					cb.invokevirtual(
						ConstantDescs.CD_String,
						"equals",
						MethodTypeDesc.of(ConstantDescs.CD_boolean, ConstantDescs.CD_Object)
					);
					storeRegister(stringEq.dest());
				}
			}
		}

		private void emitBigIntegerBinary(RegisterId dest, RegisterId lhs, RegisterId rhs, String method) {
			loadRegister(lhs);
			loadRegister(rhs);
			cb.invokevirtual(
				CD_BIG_INTEGER,
				method,
				MethodTypeDesc.of(CD_BIG_INTEGER, CD_BIG_INTEGER)
			);
			storeRegister(dest);
		}

		private void emitBigIntegerUnary(RegisterId dest, RegisterId value, String method) {
			loadRegister(value);
			cb.invokevirtual(
				CD_BIG_INTEGER,
				method,
				MethodTypeDesc.of(CD_BIG_INTEGER)
			);
			storeRegister(dest);
		}

		private void emitBigIntegerShift(RegisterId dest, RegisterId lhs, RegisterId rhs, String method) {
			loadRegister(lhs);
			loadBigIntegerAsInt(rhs);
			cb.invokevirtual(
				CD_BIG_INTEGER,
				method,
				MethodTypeDesc.of(CD_BIG_INTEGER, ConstantDescs.CD_int)
			);
			storeRegister(dest);
		}

		private void emitBigIntegerCompare(
			RegisterId dest,
			RegisterId lhs,
			RegisterId rhs,
			Consumer<Label> branch
		) {
			loadRegister(lhs);
			loadRegister(rhs);
			cb.invokevirtual(
				CD_BIG_INTEGER,
				"compareTo",
				MethodTypeDesc.of(ConstantDescs.CD_int, CD_BIG_INTEGER)
			);
			emitBooleanFromBranch(branch);
			storeRegister(dest);
		}

		private void loadBigIntegerAsInt(RegisterId register) {
			loadRegister(register);
			cb.invokevirtual(
				CD_BIG_INTEGER,
				"intValueExact",
				MethodTypeDesc.of(ConstantDescs.CD_int)
			);
		}

		private void intToBigInteger() {
			cb
				.i2l()
				.invokestatic(
					CD_BIG_INTEGER,
					"valueOf",
					MethodTypeDesc.of(CD_BIG_INTEGER, ConstantDescs.CD_long)
				);
		}

		private void emitBooleanFromBranch(Consumer<Label> branch) {
			var trueLabel = cb.newLabel();
			var endLabel = cb.newLabel();

			branch.accept(trueLabel);
			cb
				.iconst_0()
				.goto_(endLabel)
				.labelBinding(trueLabel)
				.iconst_1()
				.labelBinding(endLabel);
		}

		private TypeKind arrayElementKind(Token elementType) {
			return TypeKind.from(tokenAsClassDesc(elementType)).asLoadable();
		}

		private void boxValue(TypeKind kind) {
			switch(kind) {
				case BOOLEAN -> cb.invokestatic(
					ConstantDescs.CD_Boolean,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Boolean, ConstantDescs.CD_boolean)
				);

				case BYTE -> cb.invokestatic(
					ConstantDescs.CD_Byte,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Byte, ConstantDescs.CD_byte)
				);

				case CHAR -> cb.invokestatic(
					ConstantDescs.CD_Character,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Character, ConstantDescs.CD_char)
				);

				case SHORT -> cb.invokestatic(
					ConstantDescs.CD_Short,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Short, ConstantDescs.CD_short)
				);

				case INT -> cb.invokestatic(
					ConstantDescs.CD_Integer,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Integer, ConstantDescs.CD_int)
				);

				case LONG -> cb.invokestatic(
					ConstantDescs.CD_Long,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Long, ConstantDescs.CD_long)
				);

				case FLOAT -> cb.invokestatic(
					ConstantDescs.CD_Float,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Float, ConstantDescs.CD_float)
				);

				case DOUBLE -> cb.invokestatic(
					ConstantDescs.CD_Double,
					"valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Double, ConstantDescs.CD_double)
				);

				case REFERENCE -> {
				}

				case VOID -> throw new IllegalArgumentException("Cannot box void");
			}
		}

		private void unboxValue(TypeKind kind) {
			switch(kind) {
				case BOOLEAN -> cb
					.checkcast(ConstantDescs.CD_Boolean)
					.invokevirtual(
						ConstantDescs.CD_Boolean,
						"booleanValue",
						MethodTypeDesc.of(ConstantDescs.CD_boolean)
					);

				case BYTE -> cb
					.checkcast(ConstantDescs.CD_Byte)
					.invokevirtual(
						ConstantDescs.CD_Byte,
						"byteValue",
						MethodTypeDesc.of(ConstantDescs.CD_byte)
					);

				case CHAR -> cb
					.checkcast(ConstantDescs.CD_Character)
					.invokevirtual(
						ConstantDescs.CD_Character,
						"charValue",
						MethodTypeDesc.of(ConstantDescs.CD_char)
					);

				case SHORT -> cb
					.checkcast(ConstantDescs.CD_Short)
					.invokevirtual(
						ConstantDescs.CD_Short,
						"shortValue",
						MethodTypeDesc.of(ConstantDescs.CD_short)
					);

				case INT -> cb
					.checkcast(ConstantDescs.CD_Integer)
					.invokevirtual(
						ConstantDescs.CD_Integer,
						"intValue",
						MethodTypeDesc.of(ConstantDescs.CD_int)
					);

				case LONG -> cb
					.checkcast(ConstantDescs.CD_Long)
					.invokevirtual(
						ConstantDescs.CD_Long,
						"longValue",
						MethodTypeDesc.of(ConstantDescs.CD_long)
					);

				case FLOAT -> cb
					.checkcast(ConstantDescs.CD_Float)
					.invokevirtual(
						ConstantDescs.CD_Float,
						"floatValue",
						MethodTypeDesc.of(ConstantDescs.CD_float)
					);

				case DOUBLE -> cb
					.checkcast(ConstantDescs.CD_Double)
					.invokevirtual(
						ConstantDescs.CD_Double,
						"doubleValue",
						MethodTypeDesc.of(ConstantDescs.CD_double)
					);

				case REFERENCE -> {
				}

				case VOID -> throw new IllegalArgumentException("Cannot unbox void");
			}
		}

		private void unsupportedInstruction(Instruction instruction) {
			throw new UnsupportedOperationException(
				"Instruction is not implemented in JVM backend block emitter: "
					+ instruction.getClass().getSimpleName()
			);
		}

		private void emitRuntimeUnsupported(String message) {
			cb
				.new_(CD_UNSUPPORTED_OPERATION_EXCEPTION)
				.dup()
				.ldc(message)
				.invokespecial(
					CD_UNSUPPORTED_OPERATION_EXCEPTION,
					ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String)
				)
				.athrow();
		}

		private record BlockLabels(
			Label start,
			Label end
		) {
		}
	}
}
