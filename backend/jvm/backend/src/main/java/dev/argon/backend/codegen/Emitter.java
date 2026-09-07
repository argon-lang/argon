package dev.argon.backend.codegen;

import dev.argon.backend.ir.ClassNaming;
import dev.argon.backend.ir.TokenTypes;
import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmExtern;
import dev.argon.vm.*;
import dev.argon.vm.Instruction;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.lang.classfile.*;
import java.lang.classfile.attribute.InnerClassesAttribute;
import java.lang.classfile.attribute.InnerClassInfo;
import java.lang.classfile.attribute.ModuleAttribute;
import java.lang.classfile.attribute.PermittedSubclassesAttribute;
import java.lang.classfile.attribute.RuntimeInvisibleAnnotationsAttribute;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.DirectMethodHandleDesc;
import java.lang.constant.DynamicCallSiteDesc;
import java.lang.constant.MethodHandleDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.constant.ModuleDesc;
import java.lang.constant.PackageDesc;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

final class Emitter {
	private static final ClassFile CLASS_FILE = ClassFile.of();
	private static final String MODULE_INFO_ENTRY = "module-info.class";
	private static final ModuleDesc MD_ARGON_RUNTIME = ModuleDesc.of("dev.argon.runtime");
	private static final ClassDesc CD_LAMBDA_METAFACTORY = ClassDesc.of("java.lang.invoke.LambdaMetafactory");
	private static final ClassDesc CD_PARTIAL_APPLICATION_SUPPORT = ClassDesc
		.of("dev.argon.runtime.PartialApplicationSupport");
	private static final ClassDesc CD_ARGON_ACCESS = ClassDesc.of("dev.argon.runtime.ArgonAccess");
	private static final ClassDesc CD_ARGON_ACCESS_MODIFIER = ClassDesc.of("dev.argon.runtime.ArgonAccessModifier");
	private static final ClassDesc CD_STRING_CONCAT_FACTORY = ClassDesc.of("java.lang.invoke.StringConcatFactory");
	private static final DirectMethodHandleDesc BSM_LAMBDA_METAFACTORY = MethodHandleDesc.ofMethod(
		DirectMethodHandleDesc.Kind.STATIC, CD_LAMBDA_METAFACTORY, "metafactory",
		MethodTypeDesc.of(ConstantDescs.CD_CallSite, ConstantDescs.CD_MethodHandles_Lookup, ConstantDescs.CD_String,
			ConstantDescs.CD_MethodType, ConstantDescs.CD_MethodType, ConstantDescs.CD_MethodHandle,
			ConstantDescs.CD_MethodType));
	private static final DirectMethodHandleDesc BSM_PARTIAL_TOKEN_FUNCTION = MethodHandleDesc.ofMethod(
		DirectMethodHandleDesc.Kind.STATIC, CD_PARTIAL_APPLICATION_SUPPORT, "tokenFunction",
		MethodTypeDesc.of(ConstantDescs.CD_CallSite, ConstantDescs.CD_MethodHandles_Lookup, ConstantDescs.CD_String,
			ConstantDescs.CD_MethodType, ConstantDescs.CD_MethodHandle, ConstantDescs.CD_int));
	private static final DirectMethodHandleDesc BSM_STRING_CONCAT_WITH_CONSTANTS = MethodHandleDesc.ofMethod(
		DirectMethodHandleDesc.Kind.STATIC, CD_STRING_CONCAT_FACTORY, "makeConcatWithConstants",
		MethodTypeDesc.of(ConstantDescs.CD_CallSite, ConstantDescs.CD_MethodHandles_Lookup, ConstantDescs.CD_String,
			ConstantDescs.CD_MethodType, ConstantDescs.CD_String));

	public Emitter(ProgramModel program, ZipOutputStream zos) {
		this.program = program;
		this.zos = zos;
	}

	public void markEmitExecutable() {
		this.executable = true;
	}

	private final ProgramModel program;
	private final ZipOutputStream zos;
	private boolean executable = false;
	private boolean emittedModuleInfo = false;

	private int visibility(AccessModifierGlobal access) {
		return switch (access) {
			case AccessModifierGlobal.Public(),AccessModifierGlobal.Internal() -> ClassFile.ACC_PUBLIC;
			case AccessModifierGlobal.ModulePrivate() -> 0;
		};
	}

	private int visibility(AccessModifier access) {
		return switch (access) {
			case AccessModifier.Public(),AccessModifier.Internal(),AccessModifier.ProtectedOrInternal() ->
				ClassFile.ACC_PUBLIC;
			case AccessModifier.Private() -> ClassFile.ACC_PRIVATE;
			case AccessModifier.Protected(),AccessModifier.ProtectedAndInternal() -> ClassFile.ACC_PROTECTED;
			case AccessModifier.ModulePrivate() -> 0;
		};
	}

	private String annotationValue(AccessModifierGlobal access) {
		return switch (access) {
			case AccessModifierGlobal.Public() -> "PUBLIC";
			case AccessModifierGlobal.Internal() -> "INTERNAL";
			case AccessModifierGlobal.ModulePrivate() -> "MODULE_PRIVATE";
		};
	}

	private String annotationValue(AccessModifier access) {
		return switch (access) {
			case AccessModifier.Public() -> "PUBLIC";
			case AccessModifier.Private() -> "PRIVATE";
			case AccessModifier.Protected() -> "PROTECTED";
			case AccessModifier.Internal() -> "INTERNAL";
			case AccessModifier.ProtectedOrInternal() -> "PROTECTED_OR_INTERNAL";
			case AccessModifier.ProtectedAndInternal() -> "PROTECTED_AND_INTERNAL";
			case AccessModifier.ModulePrivate() -> "MODULE_PRIVATE";
		};
	}

	private RuntimeInvisibleAnnotationsAttribute accessAnnotation(String value) {
		return RuntimeInvisibleAnnotationsAttribute.of(Annotation.of(CD_ARGON_ACCESS,
			AnnotationElement.of("value", AnnotationValue.ofEnum(CD_ARGON_ACCESS_MODIFIER, value))));
	}

	private void emitAnnotatedMethodBody(ClassBuilder classBuilder, String name, MethodTypeDesc descriptor, int flags,
		AccessModifier access, Consumer<CodeBuilder> body) {
		classBuilder.withMethod(name, descriptor, flags,
			methodBuilder -> methodBuilder.with(accessAnnotation(annotationValue(access))).withCode(body));
	}

	public void emit() throws IOException {
		emitClassFiles();

		emitModuleInfo();

		for(var module : program.modules()) {
			emitModule(module);
		}

		if(executable) {
			emitExecutableMain();
		}
	}

	private void emitClassFiles() throws IOException {
		var requiredExports = requiredExports();
		var requiredRequires = requiredRequires();

		for(var classfile : program.decodedMetadata().platformMetadata().additionalClasses().orElse(List.of())) {
			var classModel = classfile.model();
			if(classModel.isModuleInfo()) {
				writeEntry(MODULE_INFO_ENTRY,
					moduleInfoBytesWithExportsAndRequires(classModel, requiredExports, requiredRequires));
				emittedModuleInfo = true;
			}
			else {
				writeEntry(classEntryName(classModel), classfile.toByteArray());
			}
		}
	}

	private void emitModuleInfo() throws IOException {
		if(!emittedModuleInfo) {
			var tubeInfo = program.getTubeInfo(UnsignedBigInteger.ZERO);

			var moduleAttribute = ModuleAttribute.of(tubeInfo.moduleName(), builder -> {
				builder.requires(ModuleDesc.of("java.base"), ClassFile.ACC_MANDATED, null);
				for(var moduleName : requiredRequires()) {
					builder.requires(moduleName, 0, null);
				}
				for(var packageName : requiredExports()) {
					builder.exports(packageName, 0);
				}
			});

			var miBytes = CLASS_FILE.buildModule(moduleAttribute);

			writeEntry(MODULE_INFO_ENTRY, miBytes);
			emittedModuleInfo = true;
		}
	}

	private Set<PackageDesc> requiredExports() {
		var emittedPackages = new HashSet<PackageDesc>();

		for(var classfile : program.decodedMetadata().platformMetadata().additionalClasses().orElse(List.of())) {
			var classModel = classfile.model();
			if(!classModel.isModuleInfo()) {
				var internalName = classModel.thisClass().asInternalName();
				var separator = internalName.lastIndexOf('/');
				if(separator >= 0) {
					emittedPackages.add(PackageDesc.of(internalName.substring(0, separator).replace('/', '.')));
				}
			}
		}

		for(var module : program.modules()) {
			if(module.exports().stream().anyMatch(this::isPublicTopLevelDeclaration)) {
				emittedPackages.add(program.getModuleInfo(module.moduleId()).packageName());
			}
		}

		var exports = new HashSet<PackageDesc>();

		for(var module : program.modules()) {
			var moduleInfo = program.getModuleInfo(module.moduleId());
			if(emittedPackages.contains(moduleInfo.packageName())) {
				exports.add(moduleInfo.packageName());
			}
		}

		return exports;
	}

	private boolean isPublicTopLevelDeclaration(ProgramModel.ModuleExportEntry entry) {
		return switch (entry) {
			case ProgramModel.ModuleExportEntry.FunctionDefinition(var value) ->
				value.definition().access() instanceof AccessModifierGlobal.Public;
			case ProgramModel.ModuleExportEntry.RecordDefinition(var value) ->
				value.definition().access() instanceof AccessModifierGlobal.Public;
			case ProgramModel.ModuleExportEntry.EnumDefinition(var value) ->
				value.definition().access() instanceof AccessModifierGlobal.Public;
			case ProgramModel.ModuleExportEntry.TraitDefinition(var value) ->
				value.definition().access() instanceof AccessModifierGlobal.Public;
			case ProgramModel.ModuleExportEntry.InstanceDefinition(var value) ->
				value.definition().access() instanceof AccessModifierGlobal.Public;
		};
	}

	private Set<ModuleDesc> requiredRequires() {
		var requires = new HashSet<ModuleDesc>();
		requires.add(MD_ARGON_RUNTIME);

		for(int i = 0; i < program.metadata().referencedTubes().size(); ++i) {
			requires.add(program.getTubeInfo(UnsignedBigInteger.valueOf(i + 1)).moduleName());
		}

		return requires;
	}

	private byte[] moduleInfoBytesWithExportsAndRequires(ClassModel moduleInfo, Set<PackageDesc> requiredExports,
		Set<ModuleDesc> requiredRequires) {
		var moduleAttribute = moduleInfo.findAttribute(Attributes.module())
			.orElseThrow(() -> new IllegalArgumentException("module-info classfile is missing its Module attribute"));
		var augmentedModuleAttribute = moduleAttributeWithExportsAndRequires(moduleAttribute, requiredExports,
			requiredRequires);

		var transform = ClassTransform.dropping(element -> element instanceof ModuleAttribute)
			.andThen(ClassTransform.endHandler(builder -> builder.with(augmentedModuleAttribute)));

		return CLASS_FILE.transformClass(moduleInfo, transform);
	}

	private ModuleAttribute moduleAttributeWithExportsAndRequires(ModuleAttribute moduleAttribute,
		Set<PackageDesc> requiredExports, Set<ModuleDesc> requiredRequires) {
		var existingRequires = new HashSet<ModuleDesc>();
		for(var requireInfo : moduleAttribute.requires()) {
			existingRequires.add(requireInfo.requires().asSymbol());
		}

		var existingExports = new HashSet<PackageDesc>();
		for(var exportInfo : moduleAttribute.exports()) {
			existingExports.add(exportInfo.exportedPackage().asSymbol());
		}

		return ModuleAttribute.of(moduleAttribute.moduleName(), builder -> {
			builder.moduleFlags(moduleAttribute.moduleFlagsMask());
			moduleAttribute.moduleVersion().ifPresent(version -> builder.moduleVersion(version.stringValue()));

			for(var requireInfo : moduleAttribute.requires()) {
				builder.requires(requireInfo);
			}
			for(var moduleName : requiredRequires) {
				if(!existingRequires.contains(moduleName)) {
					builder.requires(moduleName, 0, null);
				}
			}
			for(var exportInfo : moduleAttribute.exports()) {
				builder.exports(exportInfo);
			}
			for(var packageName : requiredExports) {
				if(!existingExports.contains(packageName)) {
					builder.exports(packageName, 0);
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
		});
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
			switch (entry) {
				case ProgramModel.ModuleExportEntry.FunctionDefinition(var func) -> functions.add(func);

				case ProgramModel.ModuleExportEntry.RecordDefinition(var rec) -> emitRecord(rec);

				case ProgramModel.ModuleExportEntry.EnumDefinition(var enumDef) -> emitEnum(enumDef);

				case ProgramModel.ModuleExportEntry.TraitDefinition(var traitDef) -> emitTrait(traitDef);

				case ProgramModel.ModuleExportEntry.InstanceDefinition(var instanceDef) -> emitInstance(instanceDef);
			}
		}

		if(!functions.isEmpty()) {
			var moduleInfo = program.getModuleInfo(module.moduleId());
			emitGlobalFunctions(moduleInfo, functions);
		}
	}

	private void emitGlobalFunctions(ProgramModel.ModuleInfo module, List<TubeFileEntry.FunctionDefinition> functions)
		throws IOException {
		var classDesc = ClassNaming.moduleGlobalFunctionsClassName(module);
		var bytes = CLASS_FILE.build(classDesc, classBuilder -> {
			var holderVisibility = functions.stream()
				.anyMatch(function -> visibility(function.definition().access()) != 0) ? ClassFile.ACC_PUBLIC : 0;
			classBuilder.withFlags(holderVisibility | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object);

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME, ConstantDescs.MTD_void, ClassFile.ACC_PRIVATE,
				codeBuilder -> codeBuilder.aload(0)
					.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void).return_());

			for(var entry : functions) {
				var function = entry.definition();
				var functionInfo = program.getFunctionInfo(function.functionId());

				classBuilder.withMethod(functionInfo.name(), functionInfo.descriptor(),
					visibility(entry.definition().access()) | ClassFile.ACC_STATIC,
					methodBuilder -> methodBuilder.with(accessAnnotation(annotationValue(entry.definition().access())))
						.withCode(codeBuilder -> emitGlobalFunctionBody(codeBuilder, function)));
			}
		});

		writeEntry(classEntryName(classDesc), bytes);
	}

	private void emitExecutableMain() throws IOException {
		var rootModule = program.modules().stream().filter(module -> module.moduleId().equals(UnsignedBigInteger.ZERO))
			.findAny().orElseThrow(() -> new IllegalArgumentException("Executable output requires a root module"));
		var argonMain = rootModule.exports().stream().flatMap(export -> switch (export) {
			case ProgramModel.ModuleExportEntry.FunctionDefinition(var function) -> Stream.of(function.definition());
			default -> Stream.<FunctionDefinition>of();
		}).filter(function -> {
			var functionInfo = program.getFunctionInfo(function.functionId());
			return function._import() instanceof ImportSpecifier.Global global
				&& global.name() instanceof Identifier.Named(var name) && name.equals("main")
				&& isExecutableMainDescriptor(functionInfo.descriptor());
		}).findAny().orElseThrow(() -> new IllegalArgumentException(
			"Executable output requires an exported main function with one empty tuple parameter"));
		var argonMainInfo = program.getFunctionInfo(argonMain.functionId());

		var moduleInfo = program.getModuleInfo(UnsignedBigInteger.ZERO);
		var mainClassDesc = ClassDesc.of(moduleInfo.packageName().name(), "Main");
		var bytes = CLASS_FILE.build(mainClassDesc, classBuilder -> {
			classBuilder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object);

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME, ConstantDescs.MTD_void, ClassFile.ACC_PRIVATE,
				codeBuilder -> codeBuilder.aload(0)
					.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void).return_());

			classBuilder.withMethodBody("main",
				MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String.arrayType()),
				ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
				codeBuilder -> codeBuilder.new_(ClassDesc.of("dev.argon.runtime", "Tuple0")).dup()
					.invokespecial(ClassDesc.of("dev.argon.runtime", "Tuple0"), ConstantDescs.INIT_NAME,
						ConstantDescs.MTD_void)
					.invokestatic(argonMainInfo.definingClass(), argonMainInfo.name(), argonMainInfo.descriptor())
					.invokestatic(ClassDesc.of("dev.argon.runtime", "Trampoline"), "resolve",
						MethodTypeDesc.of(ConstantDescs.CD_Object, ClassDesc.of("dev.argon.runtime", "Trampoline")),
						true)
					.pop().return_());
		});

		writeEntry(classEntryName(mainClassDesc), bytes);
	}

	private boolean isExecutableMainDescriptor(MethodTypeDesc descriptor) {
		return descriptor.parameterCount() == 1
			&& descriptor.parameterType(0).equals(ClassDesc.of("dev.argon.runtime", "Tuple0"));
	}

	private void emitGlobalFunctionBody(CodeBuilder cb, FunctionDefinition function) {
		var implementation = function.implementation();
		if(implementation.isEmpty()) {
			throw new RuntimeException("Function implementation is missing");
		}

		emitFunctionImplementationBody(cb, function.signature(), implementation.get(), Optional.empty(), index -> {
			throw new UnsupportedOperationException("Function cannot load parent token parameter");
		});
	}

	private void emitFunctionImplementationBody(CodeBuilder cb, FunctionSignature signature,
		FunctionImplementation implementation, Optional<ClassDesc> receiverType,
		BlockEmitter.ParentTokenParameterLoader parentTokenParameterLoader) {
		switch (implementation) {
			case FunctionImplementation.Extern extern -> emitExternFunctionBody(cb, signature, extern);
			case FunctionImplementation.VmIr ir -> {
				var registerOffset = 0;
				int[] tokenSlots = new int[signature.tokenParameters().size()];
				TypeKind[] tokenKinds = new TypeKind[tokenSlots.length];
				int[] registerSlots = new int[Math.addExact(
					Math.addExact(receiverType.isPresent() ? 1 : 0, signature.parameters().size()),
					ir.body().variables().variables().size())];
				Token[] registerTokens = new Token[registerSlots.length];
				ClassDesc[] registerTypes = new ClassDesc[registerSlots.length];
				TypeKind[] registerKinds = new TypeKind[registerSlots.length];

				if(receiverType.isPresent()) {
					var receiverDesc = receiverType.get();
					registerSlots[registerOffset] = cb.receiverSlot();
					registerTokens[registerOffset] = new Token.Boxed();
					registerTypes[registerOffset] = receiverDesc;
					registerKinds[registerOffset] = TypeKind.from(receiverDesc).asLoadable();
					++registerOffset;
				}

				for(int i = 0; i < signature.tokenParameters().size(); ++i) {
					var tokenType = tokenAsClassDesc(signature.tokenParameters().get(i).kind());
					tokenSlots[i] = cb.parameterSlot(i);
					tokenKinds[i] = TypeKind.from(tokenType).asLoadable();
				}

				for(int i = 0; i < signature.parameters().size(); ++i) {
					var parameterToken = signature.parameters().get(i).paramType();
					var parameterType = tokenAsClassDesc(parameterToken);
					registerSlots[registerOffset] = cb.parameterSlot(signature.tokenParameters().size() + i);
					registerTokens[registerOffset] = parameterToken;
					registerTypes[registerOffset] = parameterType;
					registerKinds[registerOffset] = TypeKind.from(parameterType).asLoadable();
					++registerOffset;
				}

				for(int i = 0; i < ir.body().variables().variables().size(); ++i) {
					var variable = ir.body().variables().variables().get(i);
					var varType = tokenAsClassDesc(variable.type());
					var varKind = TypeKind.from(varType).asLoadable();
					registerSlots[registerOffset] = cb.allocateLocal(varKind);
					registerTokens[registerOffset] = variable.type();
					registerTypes[registerOffset] = varType;
					registerKinds[registerOffset] = varKind;
					++registerOffset;
				}

				var returnToken = signature.returnType();
				var returnKind = TypeKind.from(tokenAsClassDesc(returnToken));

				var blockEmitter = new BlockEmitter(cb, tokenSlots, tokenKinds, parentTokenParameterLoader,
					registerSlots, registerTokens, registerTypes, registerKinds, returnToken, returnKind);
				blockEmitter.emitRegion(ir.body().region());
			}
		}
	}

	private void emitExternFunctionBody(CodeBuilder cb, FunctionSignature signature,
		FunctionImplementation.Extern extern) {
		var jvmFunction = decodeJvmFunction(extern);
		var externMethod = jvmFunction.name();
		var externMethodType = MethodTypeDesc.ofDescriptor(jvmFunction.descriptor());

		for(int i = 0; i < signature.tokenParameters().size(); ++i) {
			var parameterType = tokenAsClassDesc(signature.tokenParameters().get(i).kind());
			cb.loadLocal(TypeKind.from(parameterType).asLoadable(), cb.parameterSlot(i));
		}
		for(int i = 0; i < signature.parameters().size(); ++i) {
			var parameterType = tokenAsClassDesc(signature.parameters().get(i).paramType());
			var parameterKind = TypeKind.from(parameterType).asLoadable();
			cb.loadLocal(parameterKind, cb.parameterSlot(signature.tokenParameters().size() + i));
		}

		cb.invokestatic(ClassDesc.ofInternalName(jvmFunction._class()), externMethod, externMethodType).areturn();
	}

	private JvmExtern.JvmFunction decodeJvmFunction(FunctionImplementation.Extern extern) {
		try {
			return (JvmExtern.JvmFunction) JvmExtern.codec().decode(extern.extern());
		}
		catch(DecodeException ex) {
			throw new IllegalArgumentException("Invalid JVM extern function metadata", ex);
		}
	}

	private List<ClassDesc> tokenParameterDescs(FunctionSignature signature) {
		return signature.tokenParameters().stream().map(parameter -> tokenAsClassDesc(parameter.kind())).toList();
	}

	private UnsignedBigInteger getModuleId(dev.argon.vm.ImportSpecifier importSpecifier) {
		return switch (importSpecifier) {
			case dev.argon.vm.ImportSpecifier.Global global -> global.moduleId();
			case dev.argon.vm.ImportSpecifier.Local local -> getModuleId(local.parent());
		};
	}

	private void emitRecord(TubeFileEntry.RecordDefinition rec) throws IOException {
		var definition = rec.definition();
		var recordInfo = program.getRecordInfo(definition.recordId());

		var tokenParameterDescs = tokenParameterDescs(definition.signature());
		var fields = definition.fields();

		var bytes = CLASS_FILE.build(recordInfo.recordClassDesc(), classBuilder -> {
			classBuilder.withFlags(visibility(definition.access()) | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object).with(accessAnnotation(annotationValue(definition.access())));

			for(int i = 0; i < tokenParameterDescs.size(); ++i) {
				classBuilder.withField(ClassNaming.typeTokenParameterFieldName(i), tokenParameterDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(var field : fields) {
				var fieldInfo = program.getRecordFieldInfo(field.fieldId());
				classBuilder.withField(fieldInfo.fieldName(), fieldInfo.fieldType(),
					fieldBuilder -> fieldBuilder
						.withFlags(visibility(field.access()) | (field.mutable() ? 0 : ClassFile.ACC_FINAL))
						.with(accessAnnotation(annotationValue(field.access()))));
			}

			classBuilder.withMethodBody(recordInfo.builderInfo().builderMethodName(),
				recordInfo.builderInfo().builderFactoryMethodDesc(), ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
				codeBuilder -> {
					codeBuilder.new_(recordInfo.builderInfo().builderClassDesc()).dup();

					for(int i = 0; i < tokenParameterDescs.size(); ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder.loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(),
							codeBuilder.parameterSlot(i));
					}

					codeBuilder
						.invokespecial(recordInfo.builderInfo().builderClassDesc(), ConstantDescs.INIT_NAME,
							recordInfo.builderInfo().builderFactoryMethodDesc().changeReturnType(ConstantDescs.CD_void))
						.areturn();
				});

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, recordInfo.builderInfo().builderClassDesc()),
				ClassFile.ACC_PUBLIC, codeBuilder -> {
					codeBuilder.aload(0).invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME,
						ConstantDescs.MTD_void);

					for(int i = 0; i < tokenParameterDescs.size(); ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder.aload(0).aload(1)
							.getfield(recordInfo.builderInfo().builderClassDesc(),
								ClassNaming.typeTokenParameterFieldName(i), tokenParameterDesc)
							.putfield(recordInfo.recordClassDesc(), ClassNaming.typeTokenParameterFieldName(i),
								tokenParameterDesc);
					}

					for(var field : fields) {
						var fieldInfo = program.getRecordFieldInfo(field.fieldId());
						codeBuilder.aload(0).aload(1)
							.getfield(recordInfo.builderInfo().builderClassDesc(), fieldInfo.fieldName(),
								fieldInfo.fieldType())
							.putfield(recordInfo.recordClassDesc(), fieldInfo.fieldName(), fieldInfo.fieldType());
					}

					codeBuilder.return_();
				});

			for(var method : definition.methods()) {
				emitMethod(classBuilder, method, false,
					cb -> index -> cb.aload(cb.receiverSlot()).getfield(recordInfo.recordClassDesc(),
						ClassNaming.typeTokenParameterFieldName(index), tokenParameterDescs.get(index)));
			}
			for(var method : definition.staticMethods())
				emitStaticMethod(classBuilder, method);
		});

		writeEntry(classEntryName(recordInfo.recordClassDesc()), bytes);
		emitRecordBuilder(recordInfo, tokenParameterDescs, fields);
	}

	private void emitRecordBuilder(ProgramModel.RecordInfo recordInfo, List<ClassDesc> tokenParameterDescs,
		List<RecordFieldDefinition> fields) throws IOException {
		var builderInfo = recordInfo.builderInfo();
		var bytes = CLASS_FILE.build(builderInfo.builderClassDesc(), classBuilder -> {
			classBuilder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object);

			for(int i = 0; i < tokenParameterDescs.size(); ++i) {
				classBuilder.withField(ClassNaming.typeTokenParameterFieldName(i), tokenParameterDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(var field : fields) {
				var fieldInfo = program.getRecordFieldInfo(field.fieldId());
				classBuilder.withField(fieldInfo.fieldName(), fieldInfo.fieldType(), ClassFile.ACC_PUBLIC);
			}

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME,
				builderInfo.builderFactoryMethodDesc().changeReturnType(ConstantDescs.CD_void), ClassFile.ACC_PUBLIC,
				codeBuilder -> {
					codeBuilder.aload(0).invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME,
						ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < tokenParameterDescs.size(); ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						var tokenParameterKind = TypeKind.from(tokenParameterDesc).asLoadable();
						codeBuilder.aload(0).loadLocal(tokenParameterKind, localSlot).putfield(
							builderInfo.builderClassDesc(), ClassNaming.typeTokenParameterFieldName(i),
							tokenParameterDesc);
						localSlot += tokenParameterKind.slotSize();
					}

					codeBuilder.return_();
				});

			for(var field : fields) {
				var fieldInfo = program.getRecordFieldInfo(field.fieldId());
				classBuilder.withMethodBody(fieldInfo.builderMethodName(), fieldInfo.builderMethodDesc(),
					ClassFile.ACC_PUBLIC,
					codeBuilder -> codeBuilder.aload(0)
						.loadLocal(TypeKind.from(fieldInfo.fieldType()).asLoadable(), codeBuilder.parameterSlot(0))
						.putfield(builderInfo.builderClassDesc(), fieldInfo.fieldName(), fieldInfo.fieldType()).aload(0)
						.areturn());
			}

			classBuilder.withMethodBody(builderInfo.buildMethodName(), builderInfo.buildMethodDesc(),
				ClassFile.ACC_PUBLIC,
				codeBuilder -> codeBuilder.new_(recordInfo.recordClassDesc()).dup().aload(0)
					.invokespecial(recordInfo.recordClassDesc(), ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, builderInfo.builderClassDesc()))
					.areturn());
		});

		writeEntry(classEntryName(builderInfo.builderClassDesc()), bytes);
	}

	private void emitEnum(TubeFileEntry.EnumDefinition enumDef) throws IOException {
		var definition = enumDef.definition();
		var enumInfo = program.getEnumInfo(enumDef.definition().enumId());
		var signature = definition.signature();

		var tokenParameterCount = signature.tokenParameters().size();
		var tokenParameterDescs = tokenParameterDescs(signature);

		var variantDescs = definition.variants().stream()
			.map(variant -> program.getEnumVariantInfo(variant.variantId()).variantClassDesc()).toList();

		var baseBytes = CLASS_FILE.build(enumInfo.enumClassDesc(), classBuilder -> {
			classBuilder.withFlags(visibility(definition.access()) | ClassFile.ACC_ABSTRACT | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object).with(accessAnnotation(annotationValue(definition.access())))
				.with(PermittedSubclassesAttribute.ofSymbols(variantDescs))
				.with(InnerClassesAttribute.of(definition.variants().stream().map(variant -> {
					var variantInfo = program.getEnumVariantInfo(variant.variantId());
					return InnerClassInfo.of(variantInfo.variantClassDesc(), Optional.of(enumInfo.enumClassDesc()),
						Optional.of(variantInfo.nestedClassName()),
						ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL);
				}).toList()));

			for(int i = 0; i < signature.tokenParameters().size(); ++i) {
				classBuilder.withField(ClassNaming.typeTokenParameterFieldName(i), tokenParameterDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, tokenParameterDescs.toArray(ClassDesc[]::new)),
				ClassFile.ACC_PROTECTED, codeBuilder -> {
					codeBuilder.aload(0).invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME,
						ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < tokenParameterCount; ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder.aload(0).loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(), localSlot)
							.putfield(enumInfo.enumClassDesc(), ClassNaming.typeTokenParameterFieldName(i),
								tokenParameterDesc);
						localSlot += TypeKind.from(tokenParameterDesc).asLoadable().slotSize();
					}

					codeBuilder.return_();
				});

			for(var method : definition.methods()) {
				emitMethod(classBuilder, method, false,
					cb -> index -> cb.aload(cb.receiverSlot()).getfield(enumInfo.enumClassDesc(),
						ClassNaming.typeTokenParameterFieldName(index), tokenParameterDescs.get(index)));
			}
			for(var method : definition.staticMethods())
				emitStaticMethod(classBuilder, method);
		});
		writeEntry(classEntryName(enumInfo.enumClassDesc()), baseBytes);

		for(var variant : definition.variants()) {
			emitEnumVariant(enumInfo, tokenParameterDescs, definition.access(), variant);
		}
	}

	private void emitEnumVariant(ProgramModel.EnumInfo enumInfo, List<ClassDesc> enumTokenParameterDescs,
		AccessModifierGlobal enumAccess, EnumVariantDefinition variant) throws IOException {
		var variantInfo = program.getEnumVariantInfo(variant.variantId());

		var argDescs = new ArrayList<ClassDesc>();
		argDescs.addAll(tokenParameterDescs(variant.signature()));
		for(var parameter : variant.signature().parameters()) {
			argDescs.add(tokenAsClassDesc(parameter.paramType()));
		}

		var bytes = CLASS_FILE.build(variantInfo.variantClassDesc(), classBuilder -> {
			classBuilder.withFlags(visibility(enumAccess) | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(enumInfo.enumClassDesc()).with(accessAnnotation(annotationValue(enumAccess)))
				.with(InnerClassesAttribute.of(
					InnerClassInfo.of(variantInfo.variantClassDesc(), Optional.of(enumInfo.enumClassDesc()),
						Optional.of(variantInfo.nestedClassName()),
						ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL),
					InnerClassInfo.of(variantInfo.builder().builderClassDesc(),
						Optional.of(variantInfo.variantClassDesc()), Optional.of("Builder"),
						ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL)));

			for(int i = 0; i < argDescs.size(); ++i) {
				classBuilder.withField(ClassNaming.instanceParameterFieldName(i), argDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(int i = 0; i < variant.fields().size(); ++i) {
				var field = variant.fields().get(i);
				var fieldInfo = program.getRecordFieldInfo(field.fieldId());
				classBuilder.withField(fieldInfo.fieldName(), fieldInfo.fieldType(),
					fieldBuilder -> fieldBuilder
						.withFlags(visibility(field.access()) | (field.mutable() ? 0 : ClassFile.ACC_FINAL))
						.with(accessAnnotation(annotationValue(field.access()))));
			}

			for(var method : variant.methods()) {
				emitMethod(classBuilder, method, false,
					cb -> index -> cb.aload(cb.receiverSlot()).getfield(variantInfo.variantClassDesc(),
						ClassNaming.instanceParameterFieldName(index), argDescs.get(index)));
			}

			classBuilder.withMethodBody(variantInfo.builder().builderMethodName(),
				variantInfo.builder().builderFactoryMethodDesc(), ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
				codeBuilder -> {
					codeBuilder.new_(variantInfo.builder().builderClassDesc()).dup();

					for(int i = 0; i < variantInfo.builder().builderFactoryMethodDesc().parameterCount(); ++i) {
						var parameterType = variantInfo.builder().builderFactoryMethodDesc().parameterType(i);
						codeBuilder.loadLocal(TypeKind.from(parameterType).asLoadable(), codeBuilder.parameterSlot(i));
					}

					codeBuilder
						.invokespecial(variantInfo.builder().builderClassDesc(), ConstantDescs.INIT_NAME,
							variantInfo.builder().builderFactoryMethodDesc().changeReturnType(ConstantDescs.CD_void))
						.areturn();
				});

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, variantInfo.builder().builderClassDesc()),
				ClassFile.ACC_PUBLIC, codeBuilder -> {
					codeBuilder.aload(0);

					for(int i = 0; i < enumTokenParameterDescs.size(); ++i) {
						var tokenParameterDesc = enumTokenParameterDescs.get(i);
						codeBuilder.aload(1).getfield(variantInfo.builder().builderClassDesc(),
							ClassNaming.typeTokenParameterFieldName(i), tokenParameterDesc);
					}
					codeBuilder.invokespecial(enumInfo.enumClassDesc(), ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, enumTokenParameterDescs.toArray(ClassDesc[]::new)));

					for(int i = 0; i < argDescs.size(); ++i) {
						var argDesc = argDescs.get(i);
						codeBuilder.aload(0).aload(1)
							.getfield(variantInfo.builder().builderClassDesc(),
								ClassNaming.instanceParameterFieldName(i), argDesc)
							.putfield(variantInfo.variantClassDesc(), ClassNaming.instanceParameterFieldName(i),
								argDesc);
					}

					for(int i = 0; i < variant.fields().size(); ++i) {
						var field = variant.fields().get(i);
						var fieldInfo = program.getRecordFieldInfo(field.fieldId());
						codeBuilder.aload(0).aload(1)
							.getfield(variantInfo.builder().builderClassDesc(), fieldInfo.fieldName(),
								fieldInfo.fieldType())
							.putfield(variantInfo.variantClassDesc(), fieldInfo.fieldName(), fieldInfo.fieldType());
					}

					codeBuilder.return_();
				});
		});

		writeEntry(classEntryName(variantInfo.variantClassDesc()), bytes);
		emitEnumVariantBuilder(enumInfo, enumTokenParameterDescs, variantInfo, argDescs, variant.fields());
	}

	private void emitEnumVariantBuilder(ProgramModel.EnumInfo enumInfo, List<ClassDesc> enumTokenParameterDescs,
		ProgramModel.EnumVariantInfo variantInfo, List<ClassDesc> argDescs, List<RecordFieldDefinition> fields)
		throws IOException {
		var builderInfo = variantInfo.builder();
		var bytes = CLASS_FILE.build(builderInfo.builderClassDesc(), classBuilder -> {
			classBuilder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object)
				.with(InnerClassesAttribute.of(InnerClassInfo.of(variantInfo.variantClassDesc(),
					Optional.of(enumInfo.enumClassDesc()), Optional.of(variantInfo.nestedClassName()),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL),
					InnerClassInfo.of(builderInfo.builderClassDesc(), Optional.of(variantInfo.variantClassDesc()),
						Optional.of("Builder"), ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL)));

			for(int i = 0; i < enumTokenParameterDescs.size(); ++i) {
				classBuilder.withField(ClassNaming.typeTokenParameterFieldName(i), enumTokenParameterDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(int i = 0; i < argDescs.size(); ++i) {
				classBuilder.withField(ClassNaming.instanceParameterFieldName(i), argDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(var field : fields) {
				var fieldInfo = program.getRecordFieldInfo(field.fieldId());
				classBuilder.withField(fieldInfo.fieldName(), fieldInfo.fieldType(), ClassFile.ACC_PUBLIC);
			}

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME,
				builderInfo.builderFactoryMethodDesc().changeReturnType(ConstantDescs.CD_void), ClassFile.ACC_PUBLIC,
				codeBuilder -> {
					codeBuilder.aload(0).invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME,
						ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < enumTokenParameterDescs.size(); ++i) {
						var tokenParameterDesc = enumTokenParameterDescs.get(i);
						var tokenParameterKind = TypeKind.from(tokenParameterDesc).asLoadable();
						codeBuilder.aload(0).loadLocal(tokenParameterKind, localSlot).putfield(
							builderInfo.builderClassDesc(), ClassNaming.typeTokenParameterFieldName(i),
							tokenParameterDesc);
						localSlot += tokenParameterKind.slotSize();
					}

					for(int i = 0; i < argDescs.size(); ++i) {
						var argDesc = argDescs.get(i);
						var argKind = TypeKind.from(argDesc).asLoadable();
						codeBuilder.aload(0).loadLocal(argKind, localSlot).putfield(builderInfo.builderClassDesc(),
							ClassNaming.instanceParameterFieldName(i), argDesc);
						localSlot += argKind.slotSize();
					}

					codeBuilder.return_();
				});

			for(var field : fields) {
				var fieldInfo = program.getRecordFieldInfo(field.fieldId());
				classBuilder.withMethodBody(fieldInfo.builderMethodName(), fieldInfo.builderMethodDesc(),
					ClassFile.ACC_PUBLIC,
					codeBuilder -> codeBuilder.aload(0)
						.loadLocal(TypeKind.from(fieldInfo.fieldType()).asLoadable(), codeBuilder.parameterSlot(0))
						.putfield(builderInfo.builderClassDesc(), fieldInfo.fieldName(), fieldInfo.fieldType()).aload(0)
						.areturn());
			}

			classBuilder.withMethodBody(builderInfo.buildMethodName(), builderInfo.buildMethodDesc(),
				ClassFile.ACC_PUBLIC,
				codeBuilder -> codeBuilder.new_(variantInfo.variantClassDesc()).dup().aload(0)
					.invokespecial(variantInfo.variantClassDesc(), ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, builderInfo.builderClassDesc()))
					.areturn());
		});

		writeEntry(classEntryName(builderInfo.builderClassDesc()), bytes);
	}

	private void emitTrait(TubeFileEntry.TraitDefinition traitDef) throws IOException {
		var definition = traitDef.definition();
		var traitInfo = program.getTraitInfo(definition.traitId());

		var bytes = CLASS_FILE.build(traitInfo.traitDesc(), classBuilder -> {
			classBuilder.withFlags(visibility(definition.access()) | ClassFile.ACC_INTERFACE | ClassFile.ACC_ABSTRACT)
				.withSuperclass(ConstantDescs.CD_Object).with(accessAnnotation(annotationValue(definition.access())));

			var tokenParameterDescs = tokenParameterDescs(definition.signature());
			for(int i = 0; i < definition.signature().tokenParameters().size(); ++i) {
				classBuilder.withMethod(ClassNaming.typeTokenParameterFieldName(i),
					MethodTypeDesc.of(tokenParameterDescs.get(i)), ClassFile.ACC_PUBLIC | ClassFile.ACC_ABSTRACT,
					methodBuilder -> {
					});
			}

			for(var method : definition.methods()) {
				emitMethod(classBuilder, method, true,
					cb -> index -> cb.aload(cb.receiverSlot()).invokeinterface(traitInfo.traitDesc(),
						ClassNaming.typeTokenParameterFieldName(index),
						MethodTypeDesc.of(tokenParameterDescs.get(index))));
			}
			for(var method : definition.staticMethods())
				emitStaticMethod(classBuilder, method);
		});

		writeEntry(classEntryName(traitInfo.traitDesc()), bytes);
	}

	private void emitInstance(TubeFileEntry.InstanceDefinition instanceDef) throws IOException {
		var definition = instanceDef.definition();
		var instanceInfo = program.getInstanceInfo(definition.instanceId());

		if(!(definition.signature().returnType() instanceof Token.Trait traitToken)) {
			throw new IllegalArgumentException("Instance definition return type is not a trait");
		}

		var traitInfo = program.getTraitInfo(traitToken.traitId());

		var isCoreException = isCoreExceptionTrait(traitToken);
		var superclassDesc = isCoreException
			? ClassDesc.of("dev.argon.runtime.ArgonException")
			: ConstantDescs.CD_Object;

		var tokenParameterCount = definition.signature().tokenParameters().size();
		var tokenParameterDescs = tokenParameterDescs(definition.signature());
		var parameterDescs = definition.signature().parameters().stream()
			.map(parameter -> tokenAsClassDesc(parameter.paramType())).toList();

		var bytes = CLASS_FILE.build(instanceInfo.instanceClassDesc(), classBuilder -> {
			classBuilder.withFlags(visibility(definition.access()) | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(superclassDesc).withInterfaceSymbols(traitInfo.traitDesc())
				.with(accessAnnotation(annotationValue(definition.access())));

			for(int i = 0; i < tokenParameterCount; ++i) {
				classBuilder.withField(ClassNaming.typeTokenParameterFieldName(i), tokenParameterDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(int i = 0; i < parameterDescs.size(); ++i) {
				classBuilder.withField(ClassNaming.instanceParameterFieldName(i), parameterDescs.get(i),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			classBuilder.withMethodBody(ConstantDescs.INIT_NAME, instanceInfo.instanceConstructorDesc(),
				ClassFile.ACC_PUBLIC, codeBuilder -> {
					codeBuilder.aload(0).invokespecial(superclassDesc, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < tokenParameterCount; ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder.aload(0).loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(), localSlot)
							.putfield(instanceInfo.instanceClassDesc(), ClassNaming.typeTokenParameterFieldName(i),
								tokenParameterDesc);
						localSlot += TypeKind.from(tokenParameterDesc).asLoadable().slotSize();
					}

					for(int i = 0; i < parameterDescs.size(); ++i) {
						var parameterDesc = parameterDescs.get(i);
						var parameterKind = TypeKind.from(parameterDesc).asLoadable();
						codeBuilder.aload(0).loadLocal(parameterKind, localSlot).putfield(
							instanceInfo.instanceClassDesc(), ClassNaming.instanceParameterFieldName(i), parameterDesc);
						localSlot += parameterKind.slotSize();
					}

					codeBuilder.return_();
				});

			for(var method : definition.methods()) {
				emitMethod(classBuilder, method, false,
					cb -> index -> cb.aload(cb.receiverSlot()).getfield(instanceInfo.instanceClassDesc(),
						ClassNaming.typeTokenParameterFieldName(index), tokenParameterDescs.get(index)));
			}

			if(isCoreException) {
				emitArgonExceptionGetMessage(classBuilder, definition.methods());
			}
		});

		writeEntry(classEntryName(instanceInfo.instanceClassDesc()), bytes);
	}

	private void emitArgonExceptionGetMessage(ClassBuilder classBuilder, List<MethodDefinition> methods) {
		var messageMethod = methods.stream().map(MethodDefinition::methodId).map(program::getMethodInfo)
			.filter(methodInfo -> methodInfo.name() instanceof Identifier.Named(var name) && name.equals("message")
				&& methodInfo.descriptor().parameterCount() == 0)
			.findAny();

		classBuilder.withMethodBody("getMessage", MethodTypeDesc.of(ConstantDescs.CD_String), ClassFile.ACC_PUBLIC,
			codeBuilder -> {
				if(messageMethod.isPresent()) {
					var methodInfo = messageMethod.get();
					codeBuilder.aload(0)
						.invokevirtual(methodInfo.definingClass(), methodInfo.methodName(), methodInfo.descriptor())
						.invokestatic(ClassDesc.of("dev.argon.runtime", "Trampoline"), "resolve",
							MethodTypeDesc.of(ConstantDescs.CD_Object, ClassDesc.of("dev.argon.runtime", "Trampoline")),
							true)
						.checkcast(ConstantDescs.CD_String).areturn();
				}
				else {
					codeBuilder.aload(0).invokespecial(ClassDesc.of("dev.argon.runtime.ArgonException"), "getMessage",
						MethodTypeDesc.of(ConstantDescs.CD_String)).areturn();
				}
			});
	}

	private void emitMethod(ClassBuilder classBuilder, MethodDefinition method, boolean isInterface,
		Function<CodeBuilder, BlockEmitter.ParentTokenParameterLoader> parentTokenParameterLoaderProvider) {
		var methodInfo = program.getMethodInfo(method.methodId());
		var methodVisibility = isInterface
			&& (method.flags()._abstract() || !(method.access() instanceof AccessModifier.Private))
				? ClassFile.ACC_PUBLIC
				: visibility(method.access());

		if(method.flags()._abstract()) {
			classBuilder.withMethod(methodInfo.methodName(), methodInfo.descriptor(),
				methodVisibility | ClassFile.ACC_ABSTRACT,
				methodBuilder -> methodBuilder.with(accessAnnotation(annotationValue(method.access()))));
		}
		else {
			var implementation = method.implementation()
				.orElseThrow(() -> new RuntimeException("Trait method implementation is missing"));
			emitAnnotatedMethodBody(classBuilder, methodInfo.methodName(), methodInfo.descriptor(), methodVisibility,
				method.access(), cb -> emitFunctionImplementationBody(cb, method.signature(), implementation,
					Optional.of(methodInfo.definingClass()), parentTokenParameterLoaderProvider.apply(cb)));
		}
	}

	private void emitStaticMethod(ClassBuilder classBuilder, StaticMethodDefinition method) {
		var info = program.getStaticMethodInfo(method.staticMethodId());
		var implementation = method.implementation()
			.orElseThrow(() -> new RuntimeException("Static method implementation is missing"));
		var methodVisibility = info.isInterface() && !(method.access() instanceof AccessModifier.Private)
			? ClassFile.ACC_PUBLIC
			: visibility(method.access());
		emitAnnotatedMethodBody(classBuilder, info.methodName(), info.descriptor(),
			methodVisibility | ClassFile.ACC_STATIC, method.access(),
			cb -> emitFunctionImplementationBody(cb, method.signature(), implementation, Optional.empty(), index -> {
				throw new UnsupportedOperationException("Static method has no parent token parameter");
			}));
	}

	private boolean isCoreExceptionTrait(Token.Trait trait) {
		var traitInfo = program.getTraitInfo(trait.traitId());
		return switch (traitInfo.importSpecifier()) {
			case ImportSpecifier.Global global -> {
				if(!(global.name() instanceof Identifier.Named(var name)) || !name.equals("Exception")) {
					yield false;
				}

				var moduleInfo = program.getModuleInfo(global.moduleId());
				var tubeInfo = program.getTubeInfo(moduleInfo.tubeId());
				yield tubeInfo.tubeName().equals(new TubeName("Argon", List.of("Core")))
					&& moduleInfo.path().equals(new ModulePath(List.of("Exception")));
			}
			case ImportSpecifier.Local _ -> false;
		};
	}

	private ClassDesc tokenAsClassDesc(Token token) {
		return TokenTypes.tokenAsClassDesc(program, token);
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
		private static final ClassDesc CD_ARGON_EXCEPTION = ClassDesc.of("dev.argon.runtime", "ArgonException");
		private static final ClassDesc CD_FUNCTION = ClassDesc.of("dev.argon.runtime", "Function");
		private static final ClassDesc CD_FUNCTION_ERASED = ClassDesc.of("dev.argon.runtime", "FunctionErased");
		private static final ClassDesc CD_FUNCTION_TOKEN = ClassDesc.of("dev.argon.runtime", "FunctionToken");
		private static final ClassDesc CD_REF_CELL = ClassDesc.of("dev.argon.runtime", "RefCell");
		private static final ClassDesc CD_TOKEN = ClassDesc.of("dev.argon.runtime", "Token");
		private static final ClassDesc CD_TYPE_INFO = ClassDesc.of("dev.argon.runtime", "TypeInfo");
		private static final ClassDesc CD_TRAMPOLINE = ClassDesc.of("dev.argon.runtime", "Trampoline");
		private static final ClassDesc CD_TRAMPOLINE_RESULT = ClassDesc.of("dev.argon.runtime", "Trampoline$Result");
		private static final ClassDesc CD_TRAMPOLINE_THUNK = ClassDesc.of("dev.argon.runtime", "Trampoline$Thunk");
		private static final ClassDesc CD_TUPLE_BASE = ClassDesc.of("dev.argon.runtime", "TupleBase");
		private static final ClassDesc CD_TUPLE_XL = ClassDesc.of("dev.argon.runtime", "TupleXL");
		private static final ClassDesc CD_UNSUPPORTED_OPERATION_EXCEPTION = ClassDesc
			.of("java.lang.UnsupportedOperationException");

		private BlockEmitter(CodeBuilder cb, int[] tokenSlots, TypeKind[] tokenKinds,
			ParentTokenParameterLoader parentTokenParameterLoader, int[] registerSlots, Token[] registerTokens,
			ClassDesc[] registerTypes, TypeKind[] registerKinds, Token returnToken, TypeKind returnKind) {
			this.cb = cb;
			this.tokenSlots = tokenSlots;
			this.tokenKinds = tokenKinds;
			this.parentTokenParameterLoader = parentTokenParameterLoader;
			this.registerSlots = registerSlots;
			this.registerTokens = registerTokens;
			this.registerTypes = registerTypes;
			this.registerKinds = registerKinds;
			this.returnToken = returnToken;
			this.returnKind = returnKind;
		}

		private final CodeBuilder cb;
		private final int[] tokenSlots;
		private final TypeKind[] tokenKinds;
		private final ParentTokenParameterLoader parentTokenParameterLoader;
		private final int[] registerSlots;
		private final Token[] registerTokens;
		private final ClassDesc[] registerTypes;
		private final TypeKind[] registerKinds;
		private final Token returnToken;
		private final TypeKind returnKind;

		private final Map<BlockId, BlockLabels> blocks = new HashMap<>();
		private @Nullable Integer returnValueSlot = null;
		private ReturnMode returnMode = new ReturnMode.Direct();
		private boolean reachable = true;

		private void emitRegion(Region region) {
			reachable = true;
			switch (region) {
				case Region.BasicBlock basicBlock -> {
					for(var instruction : basicBlock.instructions()) {
						if(!reachable) {
							break;
						}
						emitInstruction(instruction);
					}
				}
				case Region.Block blockRegion -> {
					var start = cb.newLabel();
					var end = cb.newLabel();
					var labels = new BlockLabels(start, end);
					blocks.put(blockRegion.blockId(), labels);

					cb.labelBinding(start);
					emitRegion(blockRegion.region());
					if(blockRegion.flags().isLoop()) {
						if(reachable) {
							cb.goto_(start);
						}
						reachable = false;
					}
					else {
						labels.endReachable |= reachable;
					}

					if(labels.endReachable) {
						cb.labelBinding(end);
					}
					reachable = labels.endReachable;

					blocks.remove(blockRegion.blockId());
				}
				case Region.Finally finallyRegion -> {
					emitFinally(finallyRegion);
				}
				case Region.IfElse ifElse -> {
					var whenTrue = cb.newLabel();
					var whenFalse = cb.newLabel();
					var end = cb.newLabel();
					var trueLabels = new BlockLabels(whenTrue, whenTrue);
					var falseLabels = new BlockLabels(whenFalse, whenFalse);

					blocks.put(ifElse.whenTrueBlockId(), trueLabels);
					blocks.put(ifElse.whenFalseBlockId(), falseLabels);
					emitRegion(ifElse.condition());
					var conditionFallsThrough = reachable;
					blocks.remove(ifElse.whenTrueBlockId());
					blocks.remove(ifElse.whenFalseBlockId());

					var endReachable = false;
					var trueReachable = false;
					if(conditionFallsThrough || trueLabels.endReachable) {
						cb.labelBinding(whenTrue);
						emitRegion(ifElse.whenTrue());
						trueReachable = reachable;
					}

					if(trueReachable) {
						cb.goto_(end);
						endReachable = true;
					}
					reachable = false;

					if(falseLabels.endReachable) {
						cb.labelBinding(whenFalse);
						emitRegion(ifElse.whenFalse());
						endReachable |= reachable;
					}

					if(endReachable) {
						cb.labelBinding(end);
					}
					reachable = endReachable;
				}
				case Region.Sequence sequence -> {
					for(var subRegion : sequence.regions()) {
						if(!reachable) {
							break;
						}

						emitRegion(subRegion);
					}
				}
			}
		}

		private void emitInstruction(Instruction instruction) {
			switch (instruction) {
				case Instruction.ConstString constString -> {
					cb.ldc(constString.value());
					storeRegister(constString.dest());
				}

				case Instruction.ConstInt constInt -> {
					cb.new_(CD_BIG_INTEGER).dup().ldc(constInt.value().toString()).invokespecial(CD_BIG_INTEGER,
						ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String));
					storeRegister(constInt.dest());
				}

				case Instruction.ConstI8 constI8 -> {
					cb.ldc((int) constI8.value());
					storeRegister(constI8.dest());
				}

				case Instruction.ConstU8 constU8 -> {
					cb.ldc(Byte.toUnsignedInt(constU8.value()));
					storeRegister(constU8.dest());
				}

				case Instruction.ConstI16 constI16 -> {
					cb.ldc((int) constI16.value());
					storeRegister(constI16.dest());
				}

				case Instruction.ConstU16 constU16 -> {
					cb.ldc(Short.toUnsignedInt(constU16.value()));
					storeRegister(constU16.dest());
				}

				case Instruction.ConstI32 constI32 -> {
					cb.ldc(constI32.value());
					storeRegister(constI32.dest());
				}

				case Instruction.ConstU32 constU32 -> {
					cb.ldc((int) constU32.value());
					storeRegister(constU32.dest());
				}

				case Instruction.ConstI64 constI64 -> {
					cb.ldc(constI64.value());
					storeRegister(constI64.dest());
				}

				case Instruction.ConstU64 constU64 -> {
					cb.ldc(constU64.value());
					storeRegister(constU64.dest());
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

				case Instruction.Return ret -> emitReturn(ret);

				case Instruction.Unreachable ignored -> {
					emitRuntimeUnsupported("Unreachable instruction executed");
					reachable = false;
				}

				case Instruction.BlockBreak breakInsn -> {
					var labels = blocks.get(breakInsn.blockId());
					Objects.requireNonNull(labels, "Block labels not found");
					labels.endReachable = true;
					cb.goto_(labels.end);
					reachable = false;
				}

				case Instruction.BlockBreakIf breakIf -> {
					var labels = blocks.get(breakIf.blockId());
					Objects.requireNonNull(labels, "Block labels not found");
					labels.endReachable = true;
					cb.iload(registerSlot(breakIf.condition()));
					cb.ifne(labels.end);
				}

				case Instruction.BlockBreakUnless breakUnless -> {
					var labels = blocks.get(breakUnless.blockId());
					Objects.requireNonNull(labels, "Block labels not found");
					labels.endReachable = true;
					cb.iload(registerSlot(breakUnless.condition()));
					cb.ifeq(labels.end);
				}

				case Instruction.BlockRetry retry -> {
					var labels = blocks.get(retry.blockId());
					Objects.requireNonNull(labels, "Block labels not found");
					cb.goto_(labels.start);
					reachable = false;
				}

				case Instruction.Box box -> {
					loadRegister(box.value());
					boxValue(registerToken(box.value()));
					storeRegister(box.dest());
				}

				case Instruction.Builtin builtin -> emitBuiltin(builtin.op());

				case Instruction.EnumVariantLiteral enumVariantLiteral -> {
					var variantInfo = program.getEnumVariantInfo(enumVariantLiteral.variantId());
					var enumToken = switch (enumVariantLiteral.enumType()) {
						case Token.Enum e -> e;
						default -> throw new IllegalArgumentException("Enum variant literal type is not an enum");
					};
					var enumTokenArgs = enumToken.args();

					for(var tokenArg : enumTokenArgs) {
						emitTokenValue(tokenArg);
					}
					for(var tokenArg : enumVariantLiteral.tokenArgs()) {
						emitTokenValue(tokenArg);
					}
					for(var arg : enumVariantLiteral.args()) {
						loadRegister(arg);
					}
					cb.invokestatic(variantInfo.variantClassDesc(), variantInfo.builder().builderMethodName(),
						variantInfo.builder().builderFactoryMethodDesc());
					for(var field : enumVariantLiteral.fields()) {
						var fieldInfo = program.getRecordFieldInfo(field.fieldId());
						loadRegister(field.value());
						cb.invokevirtual(variantInfo.builder().builderClassDesc(), fieldInfo.builderMethodName(),
							fieldInfo.builderMethodDesc());
					}
					cb.invokevirtual(variantInfo.builder().builderClassDesc(), variantInfo.builder().buildMethodName(),
						variantInfo.builder().buildMethodDesc());
					storeRegister(enumVariantLiteral.dest());
				}

				case Instruction.FunctionCall call -> {
					var function = program.getFunctionInfo(call.functionId());
					List<Token> tokenArgs = call.tokenArgs();
					List<RegisterId> args = call.args();
					var owner = function.definingClass();
					var name = function.name();
					var methodType = function.descriptor();
					var parameterTypes = methodType.parameterList();

					for(var tokenArg : tokenArgs) {
						emitTokenValue(tokenArg);
					}
					for(int i = 0; i < args.size(); ++i) {
						var arg = args.get(i);
						loadRegister(arg);
						var expectedType = parameterTypes.get(tokenArgs.size() + i);
						if(expectedType.equals(ConstantDescs.CD_Object)
							&& !registerType(arg).equals(ConstantDescs.CD_Object)) {
							boxValue(registerToken(arg));
						}
					}

					emitFunctionResult(call.dest(), () -> cb.invokestatic(owner, name, methodType),
						() -> cb.invokedynamic(DynamicCallSiteDesc.of(BSM_LAMBDA_METAFACTORY, "step",
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, methodType.parameterArray()),
							MethodTypeDesc.of(CD_TRAMPOLINE),
							MethodHandleDesc.ofMethod(DirectMethodHandleDesc.Kind.STATIC, owner, name, methodType),
							MethodTypeDesc.of(CD_TRAMPOLINE))));
				}

				case Instruction.StaticMethodCall call -> {
					var method = program.getStaticMethodInfo(call.staticMethodId());
					var parameterTypes = method.descriptor().parameterList();
					var ownerTokenArgs = switch (call.ownerType()) {
						case Token.Record owner -> owner.args();
						case Token.Enum owner -> owner.args();
						case Token.Trait owner -> owner.args();
						default -> throw new IllegalArgumentException(
							"Static method owner must be a record, enum, or trait type");
					};
					for(var tokenArg : ownerTokenArgs)
						emitTokenValue(tokenArg);
					for(var tokenArg : call.tokenArgs())
						emitTokenValue(tokenArg);
					var tokenArgCount = ownerTokenArgs.size() + call.tokenArgs().size();
					for(int i = 0; i < call.args().size(); ++i) {
						var arg = call.args().get(i);
						loadRegister(arg);
						var expectedType = parameterTypes.get(tokenArgCount + i);
						if(expectedType.equals(ConstantDescs.CD_Object)
							&& !registerType(arg).equals(ConstantDescs.CD_Object)) {
							boxValue(registerToken(arg));
						}
					}
					emitFunctionResult(call.dest(),
						() -> cb.invokestatic(method.definingClass(), method.methodName(), method.descriptor(),
							method.isInterface()),
						() -> cb.invokedynamic(DynamicCallSiteDesc.of(BSM_LAMBDA_METAFACTORY, "step",
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, method.descriptor().parameterArray()),
							MethodTypeDesc.of(CD_TRAMPOLINE),
							MethodHandleDesc.ofMethod(
								method.isInterface()
									? DirectMethodHandleDesc.Kind.INTERFACE_STATIC
									: DirectMethodHandleDesc.Kind.STATIC,
								method.definingClass(), method.methodName(), method.descriptor()),
							MethodTypeDesc.of(CD_TRAMPOLINE))));
				}

				case Instruction.FunctionObjectCall call -> {
					loadRegister(call.function());
					loadRegister(call.arg());
					boxValue(registerToken(call.arg()));
					emitFunctionResult(call.dest(),
						() -> cb.invokeinterface(CD_FUNCTION, "apply",
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object)),
						() -> emitFunctionObjectCallThunk(CD_FUNCTION,
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object),
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, CD_FUNCTION, ConstantDescs.CD_Object)));
				}

				case Instruction.FunctionObjectErasedCall call -> {
					loadRegister(call.function());
					emitFunctionResult(call.dest(),
						() -> cb.invokeinterface(CD_FUNCTION_ERASED, "apply", MethodTypeDesc.of(CD_TRAMPOLINE)),
						() -> emitFunctionObjectCallThunk(CD_FUNCTION_ERASED, MethodTypeDesc.of(CD_TRAMPOLINE),
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, CD_FUNCTION_ERASED)));
				}

				case Instruction.FunctionObjectTokenCall call -> {
					loadRegister(call.function());
					emitTokenValue(call.arg());
					emitFunctionResult(call.dest(),
						() -> cb.invokeinterface(CD_FUNCTION_TOKEN, "apply",
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object)),
						() -> emitFunctionObjectCallThunk(CD_FUNCTION_TOKEN,
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object),
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, CD_FUNCTION_TOKEN, ConstantDescs.CD_Object)));
				}

				case Instruction.InstanceMethodCall call -> {
					var methodInfo = program.getMethodInfo(call.methodId());
					var ownerIsInterface = call.instanceType() instanceof Token.Trait;
					var parameterTypes = methodInfo.descriptor().parameterList();

					loadRegister(call.instanceObject());
					for(var tokenArg : call.tokenArgs()) {
						emitTokenValue(tokenArg);
					}
					for(int i = 0; i < call.args().size(); ++i) {
						var arg = call.args().get(i);
						loadRegister(arg);
						var expectedType = parameterTypes.get(call.tokenArgs().size() + i);
						if(expectedType.equals(ConstantDescs.CD_Object)
							&& !registerType(arg).equals(ConstantDescs.CD_Object)) {
							boxValue(registerToken(arg));
						}
					}

					emitFunctionResult(call.dest(), () -> {
						if(ownerIsInterface) {
							cb.invokeinterface(methodInfo.definingClass(), methodInfo.methodName(),
								methodInfo.descriptor());
						}
						else {
							cb.invokevirtual(methodInfo.definingClass(), methodInfo.methodName(),
								methodInfo.descriptor());
						}
					}, () -> cb.invokedynamic(DynamicCallSiteDesc.of(BSM_LAMBDA_METAFACTORY, "step",
						instanceMethodThunkFactoryType(methodInfo.definingClass(), methodInfo.descriptor()),
						MethodTypeDesc.of(CD_TRAMPOLINE),
						MethodHandleDesc.ofMethod(
							ownerIsInterface
								? DirectMethodHandleDesc.Kind.INTERFACE_VIRTUAL
								: DirectMethodHandleDesc.Kind.VIRTUAL,
							methodInfo.definingClass(), methodInfo.methodName(), methodInfo.descriptor()),
						MethodTypeDesc.of(CD_TRAMPOLINE))));
				}

				case Instruction.IsEnumVariantOrBreak isEnumVariantOrBreak -> {
					var variantInfo = program.getEnumVariantInfo(isEnumVariantOrBreak.variantId());
					var variantDesc = variantInfo.variantClassDesc();
					var notVariantLabels = blocks.get(isEnumVariantOrBreak.notVariantBlockId());
					Objects.requireNonNull(notVariantLabels, "Block labels not found");
					notVariantLabels.endReachable = true;

					loadRegister(isEnumVariantOrBreak.value());
					cb.instanceOf(variantDesc).ifeq(notVariantLabels.end);

					for(int i = 0; i < isEnumVariantOrBreak.args().size(); ++i) {
						var arg = isEnumVariantOrBreak.args().get(i);
						loadRegister(isEnumVariantOrBreak.value());

						ClassDesc fieldDesc;
						if(i < variantInfo.signature().tokenParameters().size()) {
							fieldDesc = tokenAsClassDesc(variantInfo.signature().tokenParameters().get(i).kind());
						}
						else {
							fieldDesc = tokenAsClassDesc(variantInfo.signature().parameters()
								.get(i - variantInfo.signature().tokenParameters().size()).paramType());
						}

						cb.checkcast(variantDesc).getfield(variantDesc, ClassNaming.instanceParameterFieldName(i),
							fieldDesc);

						unboxValueFrom(fieldDesc, registerToken(arg));
						storeRegister(arg);
					}

					for(var fieldExtractor : isEnumVariantOrBreak.fieldExtractors()) {
						var fieldInfo = program.getRecordFieldInfo(fieldExtractor.fieldId());
						loadRegister(isEnumVariantOrBreak.value());
						cb.checkcast(variantDesc).getfield(variantDesc, fieldInfo.fieldName(), fieldInfo.fieldType());
						storeRegister(fieldExtractor.r());
					}
				}
				case Instruction.LoadInstanceField loadInstanceField -> {
					if(!(loadInstanceField.instanceType() instanceof Token.InstanceType instanceType)) {
						throw new IllegalArgumentException(
							"LoadInstanceField instruction has non-instance type: " + loadInstanceField.instanceType());
					}

					var instanceInfo = program.getInstanceInfo(instanceType.instanceId());
					loadRegister(loadInstanceField.instanceObject());
					cb.getfield(instanceInfo.instanceClassDesc(),
						ClassNaming.instanceParameterFieldName(
							loadInstanceField.parameterIndex().toBigInteger().intValueExact()),
						registerType(loadInstanceField.dest()));
					storeRegister(loadInstanceField.dest());
				}

				case Instruction.LoadReference loadReference -> {
					loadRegister(loadReference.ref());
					cb.invokevirtual(CD_REF_CELL, "get", MethodTypeDesc.of(ConstantDescs.CD_Object));
					unboxValue(registerToken(loadReference.dest()));
					storeRegister(loadReference.dest());
				}

				case Instruction.LoadToken loadToken -> {
					emitTokenValue(loadToken.token());
					storeRegister(loadToken.dest());
				}

				case Instruction.NewInstance newInstance -> {
					var instanceInfo = program.getInstanceInfo(newInstance.instanceId());

					cb.new_(instanceInfo.instanceClassDesc()).dup();
					for(var tokenArg : newInstance.tokenArgs()) {
						emitTokenValue(tokenArg);
					}
					for(var arg : newInstance.args()) {
						loadRegister(arg);
					}
					cb.invokespecial(instanceInfo.instanceClassDesc(), ConstantDescs.INIT_NAME,
						instanceInfo.instanceConstructorDesc());
					storeRegister(newInstance.dest());
				}

				case Instruction.NewReference newReference -> {
					cb.new_(CD_REF_CELL).dup();
					loadRegister(newReference.value());
					boxValue(registerToken(newReference.value()));
					cb.invokespecial(CD_REF_CELL, ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object));
					storeRegister(newReference.dest());
				}
				case Instruction.PartiallyAppliedFunction partial -> emitPartiallyAppliedFunction(partial.functionId(),
					partial.dest(), partial.tokenArgs(), partial.args(), PartialApplicationKind.FUNCTION);
				case Instruction.PartiallyAppliedFunctionErased partial ->
					emitPartiallyAppliedFunction(partial.functionId(), partial.dest(), partial.tokenArgs(),
						partial.args(), PartialApplicationKind.ERASED);
				case Instruction.PartiallyAppliedTokenFunction partial ->
					emitPartiallyAppliedFunction(partial.functionId(), partial.dest(), partial.tokenArgs(),
						partial.args(), PartialApplicationKind.TOKEN);
				case Instruction.Raise raise -> {
					loadRegister(raise.exception());
					cb.checkcast(CD_ARGON_EXCEPTION).athrow();
					reachable = false;
				}
				case Instruction.RecordFieldLoad recordFieldLoad -> {
					var fieldInfo = program.getRecordFieldInfo(recordFieldLoad.fieldId());
					var owner = recordFieldOwner(fieldInfo);
					loadRegister(recordFieldLoad.recordValue());
					cb.getfield(owner, fieldInfo.fieldName(), fieldInfo.fieldType());
					unboxValueFrom(fieldInfo.fieldType(), registerToken(recordFieldLoad.dest()));
					storeRegister(recordFieldLoad.dest());
				}

				case Instruction.RecordFieldStore recordFieldStore -> {
					var fieldInfo = program.getRecordFieldInfo(recordFieldStore.fieldId());
					var owner = recordFieldOwner(fieldInfo);
					loadRegister(recordFieldStore.recordValue());
					loadRegister(recordFieldStore.fieldValue());
					cb.putfield(owner, fieldInfo.fieldName(), fieldInfo.fieldType());
				}

				case Instruction.RecordLiteral recordLiteral -> {
					var record = switch (recordLiteral.recordType()) {
						case Token.Record r -> r;
						default -> throw new IllegalArgumentException("Record literal type is not a record");
					};
					var recordTokenArgs = record.args();
					var recordInfo = program.getRecordInfo(record.recordId());

					for(var tokenArg : recordTokenArgs) {
						emitTokenValue(tokenArg);
					}
					cb.invokestatic(recordInfo.recordClassDesc(), recordInfo.builderInfo().builderMethodName(),
						recordInfo.builderInfo().builderFactoryMethodDesc());
					for(var field : recordLiteral.fields()) {
						var fieldInfo = program.getRecordFieldInfo(field.fieldId());
						loadRegister(field.value());
						cb.invokevirtual(recordInfo.builderInfo().builderClassDesc(), fieldInfo.builderMethodName(),
							fieldInfo.builderMethodDesc());
					}
					cb.invokevirtual(recordInfo.builderInfo().builderClassDesc(),
						recordInfo.builderInfo().buildMethodName(), recordInfo.builderInfo().buildMethodDesc());
					storeRegister(recordLiteral.dest());
				}
				case Instruction.Tuple tuple -> {
					emitTupleValue(tuple.values());
					storeRegister(tuple.dest());
				}

				case Instruction.TupleElement tupleElement -> {
					loadRegister(tupleElement.src());
					emitTupleElement(tupleElement.src(), tupleElement.elementIndex().toBigInteger().intValueExact());
					unboxValue(registerToken(tupleElement.dest()));
					storeRegister(tupleElement.dest());
				}

				case Instruction.Unbox unbox -> {
					loadRegister(unbox.value());
					unboxValue(unbox.type());
					storeRegister(unbox.dest());
				}

				case Instruction.UpdateReference updateReference -> {
					loadRegister(updateReference.ref());
					loadRegister(updateReference.value());
					boxValue(registerToken(updateReference.value()));
					cb.invokevirtual(CD_REF_CELL, "set",
						MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object));
				}
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

		private ClassDesc registerType(RegisterId register) {
			return registerTypes[register.id().toBigInteger().intValueExact()];
		}

		private Token registerToken(RegisterId register) {
			return registerTokens[register.id().toBigInteger().intValueExact()];
		}

		private void emitTupleValue(List<RegisterId> values) {
			var tupleDesc = tupleClassDesc(values.size());
			cb.new_(tupleDesc).dup();

			if(values.size() <= 10) {
				for(var value : values) {
					loadRegister(value);
					boxValue(registerToken(value));
				}
				cb.invokespecial(tupleDesc, ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void,
					Collections.nCopies(values.size(), ConstantDescs.CD_Object).toArray(ClassDesc[]::new)));
			}
			else {
				for(int i = 0; i < 10; ++i) {
					loadRegister(values.get(i));
					boxValue(registerToken(values.get(i)));
				}
				emitTupleValue(values.subList(10, values.size()));
				var parameterTypes = new ArrayList<ClassDesc>(Collections.nCopies(10, ConstantDescs.CD_Object));
				parameterTypes.add(CD_TUPLE_BASE);
				cb.invokespecial(tupleDesc, ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, parameterTypes.toArray(ClassDesc[]::new)));
			}
		}

		private void emitTupleElement(RegisterId src, int index) {
			var tupleToken = registerToken(src);
			if(!(tupleToken instanceof Token.Tuple tuple)) {
				throw new IllegalArgumentException("TupleElement source register is not a tuple");
			}

			int remainingIndex = index;
			int remainingSize = tuple.elements().size();
			while (remainingIndex >= 10) {
				cb.checkcast(CD_TUPLE_XL).invokevirtual(CD_TUPLE_XL, "rest", MethodTypeDesc.of(CD_TUPLE_BASE));
				remainingIndex -= 10;
				remainingSize -= 10;
			}

			var tupleDesc = tupleClassDesc(remainingSize);
			cb.checkcast(tupleDesc).invokevirtual(tupleDesc, "element" + remainingIndex,
				MethodTypeDesc.of(ConstantDescs.CD_Object));
		}

		private ClassDesc tupleClassDesc(int size) {
			if(size > 10) {
				return CD_TUPLE_XL;
			}

			return ClassDesc.of("dev.argon.runtime", "Tuple" + size);
		}

		private ClassDesc recordFieldOwner(ProgramModel.RecordFieldInfo fieldInfo) {
			return switch (fieldInfo.ownerType()) {
				case RECORD -> {
					var recordInfo = program.getRecordInfo(fieldInfo.recordId());
					yield recordInfo.recordClassDesc();
				}

				case ENUM_VARIANT ->
					throw new UnsupportedOperationException("Enum variant record fields are not implemented");
			};
		}

		private void emitPartiallyAppliedFunction(UnsignedBigInteger functionId, RegisterId dest, List<Token> tokenArgs,
			List<RegisterId> args, PartialApplicationKind kind) {
			var function = program.getFunctionInfo(functionId);
			var owner = function.definingClass();
			var name = function.name();
			var interfaceDesc = partialApplicationInterface(kind);
			var nextArgType = partialApplicationNextArgType(kind, dest);
			var implementationMethodType = function.descriptor();
			var factoryType = partialApplicationFactoryType(interfaceDesc, function.signature(), tokenArgs, args);
			var targetHandle = MethodHandleDesc.ofMethod(DirectMethodHandleDesc.Kind.STATIC, owner, name,
				implementationMethodType);

			for(var tokenArg : tokenArgs) {
				emitTokenValue(tokenArg);
			}
			for(var arg : args) {
				loadRegister(arg);
			}

			var callSite = switch (kind) {
				case FUNCTION -> DynamicCallSiteDesc.of(BSM_LAMBDA_METAFACTORY, "apply", factoryType,
					MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object), targetHandle,
					MethodTypeDesc.of(CD_TRAMPOLINE, nextArgType.orElseThrow()));
				case TOKEN -> DynamicCallSiteDesc.of(BSM_PARTIAL_TOKEN_FUNCTION, "apply", factoryType, targetHandle,
					Integer.valueOf(tokenArgs.size()));
				case ERASED -> DynamicCallSiteDesc.of(BSM_LAMBDA_METAFACTORY, "apply", factoryType,
					MethodTypeDesc.of(CD_TRAMPOLINE), targetHandle, MethodTypeDesc.of(CD_TRAMPOLINE));
			};
			cb.invokedynamic(callSite);
			storeRegister(dest);
		}

		private ClassDesc partialApplicationInterface(PartialApplicationKind kind) {
			return switch (kind) {
				case FUNCTION -> CD_FUNCTION;
				case TOKEN -> CD_FUNCTION_TOKEN;
				case ERASED -> CD_FUNCTION_ERASED;
			};
		}

		private Optional<ClassDesc> partialApplicationNextArgType(PartialApplicationKind kind, RegisterId dest) {
			return switch (kind) {
				case FUNCTION -> {
					if(!(registerToken(dest) instanceof Token.Function function)) {
						throw new IllegalArgumentException("PartiallyAppliedFunction destination is not a function");
					}
					yield Optional.of(tokenAsClassDesc(function.input()));
				}
				case TOKEN, ERASED -> Optional.empty();
			};
		}

		private MethodTypeDesc partialApplicationFactoryType(ClassDesc interfaceDesc, FunctionSignature signature,
			List<Token> tokenArgs, List<RegisterId> args) {
			var parameterTypes = new ArrayList<ClassDesc>();
			for(int i = 0; i < tokenArgs.size(); ++i) {
				parameterTypes.add(tokenAsClassDesc(signature.tokenParameters().get(i).kind()));
			}
			for(var arg : args) {
				parameterTypes.add(registerType(arg));
			}
			return MethodTypeDesc.of(interfaceDesc, parameterTypes);
		}

		private MethodTypeDesc instanceMethodThunkFactoryType(ClassDesc owner, MethodTypeDesc methodType) {
			var parameterTypes = new ArrayList<ClassDesc>();
			parameterTypes.add(owner);
			parameterTypes.addAll(List.of(methodType.parameterArray()));
			return MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, parameterTypes);
		}

		private void emitFunctionResult(FunctionResult dest, Runnable emitCall, Runnable emitThunk) {
			switch (dest) {
				case FunctionResult.Discard ignored -> {
					emitCall.run();
					emitResolveTrampoline();
					cb.pop();
				}

				case FunctionResult.Register(var id) -> {
					emitCall.run();
					emitResolveTrampoline();
					unboxValue(registerToken(id));
					storeRegister(id);
				}

				case FunctionResult.ReturnValue ignored -> {
					emitThunk.run();
					cb.areturn();
					reachable = false;
				}
			}
		}

		private void emitResolveTrampoline() {
			cb.invokestatic(CD_TRAMPOLINE, "resolve", MethodTypeDesc.of(ConstantDescs.CD_Object, CD_TRAMPOLINE), true);
		}

		private void emitFunctionObjectCallThunk(ClassDesc owner, MethodTypeDesc applyMethodType,
			MethodTypeDesc thunkFactoryType) {
			cb.invokedynamic(DynamicCallSiteDesc.of(BSM_LAMBDA_METAFACTORY, "step", thunkFactoryType,
				MethodTypeDesc.of(CD_TRAMPOLINE), MethodHandleDesc
					.ofMethod(DirectMethodHandleDesc.Kind.INTERFACE_VIRTUAL, owner, "apply", applyMethodType),
				MethodTypeDesc.of(CD_TRAMPOLINE)));
		}

		private void emitTokenValue(Token token) {
			switch (token) {
				case Token.TokenParameter tokenParameter -> {
					var index = tokenParameter.index().toBigInteger().intValueExact();
					cb.loadLocal(tokenKinds[index], tokenSlots[index]);
				}
				case Token.ParentTokenParameter parentTokenParameter ->
					parentTokenParameterLoader.load(parentTokenParameter.index().toBigInteger().intValueExact());

				case Token.InstanceValue instanceValue -> {
					var instanceInfo = program.getInstanceInfo(instanceValue.instanceId());

					cb.new_(instanceInfo.instanceClassDesc()).dup();
					for(var tokenArg : instanceValue.args()) {
						emitTokenValue(tokenArg);
					}
					cb.invokespecial(instanceInfo.instanceClassDesc(), ConstantDescs.INIT_NAME,
						instanceInfo.instanceConstructorDesc());
				}

				case Token.Builtin builtinToken -> {
					var builtin = builtinToken.b();
					switch (builtin) {
						case BuiltinType.Array(var elementType) -> {
							if(TokenTypes.elementTypeRequiresErasedArray(elementType)) {
								cb.new_(CD_TYPE_INFO).dup();

								emitTokenValue(elementType);
								cb.invokevirtual(CD_TYPE_INFO, "javaClass", MethodTypeDesc.of(ConstantDescs.CD_Class));
								cb.invokevirtual(ConstantDescs.CD_Class, "arrayType",
									MethodTypeDesc.of(ConstantDescs.CD_Class));

								cb.invokespecial(CD_TYPE_INFO, ConstantDescs.INIT_NAME,
									MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Class));
							}
							else {
								emitTypeInfoTokenValue(token, List.of());
							}
						}
						case BuiltinType.Bool(),BuiltinType.Int _,BuiltinType.Never(),BuiltinType.String() ->
							emitTypeInfoTokenValue(token, List.of());
						case BuiltinType.Conjunction _,BuiltinType.Disjunction _ ->
							throw new UnsupportedOperationException(
								"TypeInfo is not implemented for JVM backend token: "
									+ builtin.getClass().getSimpleName());
					}
				}
				case Token.Boxed _,Token.TypeInfo() -> emitTypeInfoTokenValue(token, List.of());
				case Token.Enum enumToken -> emitTypeInfoTokenValue(token, enumToken.args());
				case Token.Function function ->
					emitTypeInfoTokenValue(token, List.of(function.input(), function.output()));
				case Token.FunctionErased function -> emitTypeInfoTokenValue(token, List.of(function.output()));
				case Token.FunctionToken function ->
					emitTypeInfoTokenValue(token, List.of(function.tokenKind(), function.output()));
				case Token.InstanceType instanceType -> emitTypeInfoTokenValue(token, instanceType.args());
				case Token.Record record -> emitTypeInfoTokenValue(token, record.args());
				case Token.RefCell refCell -> emitTypeInfoTokenValue(token, List.of(refCell.inner()));
				case Token.Trait trait -> emitTypeInfoTokenValue(token, trait.args());
				case Token.Tuple tuple -> emitTypeInfoTokenValue(token, tuple.elements());
			}
		}

		private void emitTypeInfoTokenValue(Token token, List<Token> args) {
			cb.new_(CD_TYPE_INFO).dup().ldc(tokenAsClassDesc(token));

			if(args.isEmpty()) {
				cb.invokespecial(CD_TYPE_INFO, ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Class));
			}
			else {
				cb.loadConstant(args.size());
				cb.anewarray(CD_TOKEN);
				for(int i = 0; i < args.size(); ++i) {
					cb.dup().loadConstant(i);
					emitTokenValue(args.get(i));
					cb.aastore();
				}
				cb.invokespecial(CD_TYPE_INFO, ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Class, CD_TOKEN.arrayType()));
			}
		}

		private void emitBuiltin(BuiltinOp op) {
			switch (op) {
				case BuiltinOp.ArrayCreateUnsafeUninitialized arrayCreate -> {
					if(TokenTypes.elementTypeRequiresErasedArray(arrayCreate.elementType())) {
						emitTokenValue(arrayCreate.elementType());

						cb.invokevirtual(CD_TYPE_INFO, "javaClass", MethodTypeDesc.of(ConstantDescs.CD_Class));

						loadBigIntegerAsInt(arrayCreate.length());

						cb.invokestatic(ClassDesc.of("java.lang.reflect.Array"), "newInstance",
							MethodTypeDesc.of(ConstantDescs.CD_Object, ConstantDescs.CD_Class, ConstantDescs.CD_int));
					}
					else {
						var elementType = tokenAsClassDesc(arrayCreate.elementType());
						var elementKind = TypeKind.from(elementType);

						loadBigIntegerAsInt(arrayCreate.length());
						if(elementKind == TypeKind.REFERENCE) {
							cb.anewarray(elementType);
						}
						else {
							cb.newarray(elementKind);
						}
					}
					storeRegister(arrayCreate.dest());
				}

				case BuiltinOp.ArrayGet arrayGet -> {
					loadRegister(arrayGet.array());
					loadBigIntegerAsInt(arrayGet.index());
					if(TokenTypes.elementTypeRequiresErasedArray(arrayGet.elementType())) {
						cb.invokestatic(ClassDesc.of("java.lang.reflect.Array"), "get",
							MethodTypeDesc.of(ConstantDescs.CD_Object, ConstantDescs.CD_Object, ConstantDescs.CD_int));
					}
					else {
						cb.arrayLoad(arrayElementKind(arrayGet.elementType()));
					}
					storeRegister(arrayGet.dest());
				}

				case BuiltinOp.ArrayLength arrayLength -> {
					loadRegister(arrayLength.array());
					if(TokenTypes.elementTypeRequiresErasedArray(arrayLength.elementType())) {
						cb.invokestatic(ClassDesc.of("java.lang.reflect.Array"), "getLength",
							MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_Object));
					}
					else {
						cb.arraylength();
					}

					intToBigInteger();
					storeRegister(arrayLength.dest());
				}

				case BuiltinOp.ArraySet arraySet -> {
					loadRegister(arraySet.array());
					loadBigIntegerAsInt(arraySet.index());
					loadRegister(arraySet.value());
					if(TokenTypes.elementTypeRequiresErasedArray(arraySet.elementType())) {
						cb.invokestatic(ClassDesc.of("java.lang.reflect.Array"), "set",
							MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object, ConstantDescs.CD_int,
								ConstantDescs.CD_Object));
					}
					else {
						cb.arrayStore(arrayElementKind(arraySet.elementType()));
					}
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
					emitIntegerBinary(intAdd.dest(), intAdd.lhs(), intAdd.rhs(), intAdd.integerType(), "add", () -> {
						cb.iadd();
						cb.i2b();
					}, () -> {
						cb.iadd();
						maskU8();
					}, () -> {
						cb.iadd();
						cb.i2s();
					}, () -> {
						cb.iadd();
						maskU16();
					}, cb::iadd, cb::iadd, cb::ladd, cb::ladd);

				case BuiltinOp.IntBitAnd intBitAnd ->
					emitIntegerBinary(intBitAnd.dest(), intBitAnd.lhs(), intBitAnd.rhs(), intBitAnd.integerType(),
						"and", cb::iand, cb::iand, cb::iand, cb::iand, cb::iand, cb::iand, cb::land, cb::land);

				case BuiltinOp.IntBitNot intBitNot ->
					emitIntegerUnary(intBitNot.dest(), intBitNot.value(), intBitNot.integerType(), "not", () -> {
						cb.loadConstant(-1);
						cb.ixor();
					}, () -> {
						cb.loadConstant(0xFF);
						cb.ixor();
					}, () -> {
						cb.loadConstant(-1);
						cb.ixor();
					}, () -> {
						cb.loadConstant(0xFFFF);
						cb.ixor();
					}, () -> {
						cb.loadConstant(-1);
						cb.ixor();
					}, () -> {
						cb.loadConstant(-1);
						cb.ixor();
					}, () -> {
						cb.loadConstant(-1L);
						cb.lxor();
					}, () -> {
						cb.loadConstant(-1L);
						cb.lxor();
					});

				case BuiltinOp.IntConvert intConvert -> emitIntConvert(intConvert.dest(), intConvert.value(),
					intConvert.sourceType(), intConvert.destType());

				case BuiltinOp.IntBitOr intBitOr ->
					emitIntegerBinary(intBitOr.dest(), intBitOr.lhs(), intBitOr.rhs(), intBitOr.integerType(), "or",
						cb::ior, cb::ior, cb::ior, cb::ior, cb::ior, cb::ior, cb::lor, cb::lor);

				case BuiltinOp.IntBitShiftLeft intShiftLeft -> emitIntegerShift(intShiftLeft.dest(), intShiftLeft.lhs(),
					intShiftLeft.rhs(), intShiftLeft.integerType(), true);

				case BuiltinOp.IntBitShiftRight intShiftRight -> emitIntegerShift(intShiftRight.dest(),
					intShiftRight.lhs(), intShiftRight.rhs(), intShiftRight.integerType(), false);

				case BuiltinOp.IntBitXor intBitXor ->
					emitIntegerBinary(intBitXor.dest(), intBitXor.lhs(), intBitXor.rhs(), intBitXor.integerType(),
						"xor", cb::ixor, cb::ixor, cb::ixor, cb::ixor, cb::ixor, cb::ixor, cb::lxor, cb::lxor);

				case BuiltinOp.IntEq intEq -> emitIntegerCompare(intEq.dest(), intEq.lhs(), intEq.rhs(),
					intEq.integerType(), cb::ifeq, cb::if_icmpeq);

				case BuiltinOp.IntGe intGe -> emitIntegerCompare(intGe.dest(), intGe.lhs(), intGe.rhs(),
					intGe.integerType(), cb::ifge, cb::if_icmpge);

				case BuiltinOp.IntGt intGt -> emitIntegerCompare(intGt.dest(), intGt.lhs(), intGt.rhs(),
					intGt.integerType(), cb::ifgt, cb::if_icmpgt);

				case BuiltinOp.IntLe intLe -> emitIntegerCompare(intLe.dest(), intLe.lhs(), intLe.rhs(),
					intLe.integerType(), cb::ifle, cb::if_icmple);

				case BuiltinOp.IntLt intLt -> emitIntegerCompare(intLt.dest(), intLt.lhs(), intLt.rhs(),
					intLt.integerType(), cb::iflt, cb::if_icmplt);

				case BuiltinOp.IntMul intMul -> emitIntegerBinary(intMul.dest(), intMul.lhs(), intMul.rhs(),
					intMul.integerType(), "multiply", () -> {
						cb.imul();
						cb.i2b();
					}, () -> {
						cb.imul();
						maskU8();
					}, () -> {
						cb.imul();
						cb.i2s();
					}, () -> {
						cb.imul();
						maskU16();
					}, cb::imul, cb::imul, cb::lmul, cb::lmul);

				case BuiltinOp.IntNegate intNegate ->
					emitIntegerUnary(intNegate.dest(), intNegate.value(), intNegate.integerType(), "negate", () -> {
						cb.ineg();
						cb.i2b();
					}, () -> {
						throw new UnsupportedOperationException("Negation not supported for unsigned integers");
					}, () -> {
						cb.ineg();
						cb.i2s();
					}, () -> {
						throw new UnsupportedOperationException("Negation not supported for unsigned integers");
					}, cb::ineg, () -> {
						throw new UnsupportedOperationException("Negation not supported for unsigned integers");
					}, cb::lneg, () -> {
						throw new UnsupportedOperationException("Negation not supported for unsigned integers");
					});

				case BuiltinOp.IntSub intSub -> emitIntegerBinary(intSub.dest(), intSub.lhs(), intSub.rhs(),
					intSub.integerType(), "subtract", () -> {
						cb.isub();
						cb.i2b();
					}, () -> {
						cb.isub();
						maskU8();
					}, () -> {
						cb.isub();
						cb.i2s();
					}, () -> {
						cb.isub();
						maskU16();
					}, cb::isub, cb::isub, cb::lsub, cb::lsub);

				case BuiltinOp.StringConcat stringConcat -> {
					if(stringConcat.args().isEmpty()) {
						cb.ldc("");
					}
					else if(stringConcat.args().size() == 1) {
						loadRegister(stringConcat.args().get(0));
					}
					else if(stringConcat.args().size() == 2) {
						loadRegister(stringConcat.args().get(0));
						loadRegister(stringConcat.args().get(1));
						cb.invokevirtual(ConstantDescs.CD_String, "concat",
							MethodTypeDesc.of(ConstantDescs.CD_String, ConstantDescs.CD_String));
					}
					else {
						for(var arg : stringConcat.args()) {
							loadRegister(arg);
						}

						var argTypes = Collections.nCopies(stringConcat.args().size(), ConstantDescs.CD_String)
							.toArray(ClassDesc[]::new);
						cb.invokedynamic(DynamicCallSiteDesc.of(BSM_STRING_CONCAT_WITH_CONSTANTS,
							"makeConcatWithConstants", MethodTypeDesc.of(ConstantDescs.CD_String, argTypes),
							"\u0001".repeat(stringConcat.args().size())));
					}
					storeRegister(stringConcat.dest());
				}

				case BuiltinOp.StringEq stringEq -> {
					loadRegister(stringEq.lhs());
					loadRegister(stringEq.rhs());
					cb.invokevirtual(ConstantDescs.CD_String, "equals",
						MethodTypeDesc.of(ConstantDescs.CD_boolean, ConstantDescs.CD_Object));
					storeRegister(stringEq.dest());
				}
			}
		}

		private void emitIntegerUnary(RegisterId dest, RegisterId value, IntegerType integerType,
			String bigIntegerMethod, Runnable i8Op, Runnable u8Op, Runnable i16Op, Runnable u16Op, Runnable i32Op,
			Runnable u32Op, Runnable i64Op, Runnable u64Op) {
			switch (integerType) {
				case INT -> {
					loadRegister(value);
					cb.invokevirtual(CD_BIG_INTEGER, bigIntegerMethod, MethodTypeDesc.of(CD_BIG_INTEGER));
					storeRegister(dest);
				}
				case I8 -> {
					loadRegister(value);
					i8Op.run();
					storeRegister(dest);
				}
				case U8 -> {
					loadRegister(value);
					u8Op.run();
					storeRegister(dest);
				}
				case I16 -> {
					loadRegister(value);
					i16Op.run();
					storeRegister(dest);
				}
				case U16 -> {
					loadRegister(value);
					u16Op.run();
					storeRegister(dest);
				}
				case I32 -> {
					loadRegister(value);
					i32Op.run();
					storeRegister(dest);
				}
				case U32 -> {
					loadRegister(value);
					u32Op.run();
					storeRegister(dest);
				}
				case I64 -> {
					loadRegister(value);
					i64Op.run();
					storeRegister(dest);
				}
				case U64 -> {
					loadRegister(value);
					u64Op.run();
					storeRegister(dest);
				}
			}
		}

		private void emitIntegerBinary(RegisterId dest, RegisterId lhs, RegisterId rhs, IntegerType integerType,
			String bigIntegerMethod, Runnable i8Op, Runnable u8Op, Runnable i16Op, Runnable u16Op, Runnable i32Op,
			Runnable u32Op, Runnable i64Op, Runnable u64Op) {
			switch (integerType) {
				case INT -> {
					loadRegister(lhs);
					loadRegister(rhs);
					cb.invokevirtual(CD_BIG_INTEGER, bigIntegerMethod,
						MethodTypeDesc.of(CD_BIG_INTEGER, CD_BIG_INTEGER));
					storeRegister(dest);
				}
				case I8 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					i8Op.run();
					storeRegister(dest);
				}
				case U8 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					u8Op.run();
					storeRegister(dest);
				}
				case I16 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					i16Op.run();
					storeRegister(dest);
				}
				case U16 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					u16Op.run();
					storeRegister(dest);
				}
				case I32 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					i32Op.run();
					storeRegister(dest);
				}
				case U32 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					u32Op.run();
					storeRegister(dest);
				}
				case I64 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					i64Op.run();
					storeRegister(dest);
				}
				case U64 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					u64Op.run();
					storeRegister(dest);
				}
			}
		}

		private void emitIntConvert(RegisterId dest, RegisterId value, IntegerType sourceType, IntegerType destType) {
			loadRegister(value);
			switch (sourceType) {
				case INT -> {
					switch (destType) {
						case INT -> {
						}
						case I8 -> {
							cb.invokevirtual(CD_BIG_INTEGER, "intValue", MethodTypeDesc.of(ConstantDescs.CD_int));
							cb.i2b();
						}
						case U8 -> {
							cb.invokevirtual(CD_BIG_INTEGER, "intValue", MethodTypeDesc.of(ConstantDescs.CD_int));
							maskU8();
						}
						case I16 -> {
							cb.invokevirtual(CD_BIG_INTEGER, "intValue", MethodTypeDesc.of(ConstantDescs.CD_int));
							cb.i2s();
						}
						case U16 -> {
							cb.invokevirtual(CD_BIG_INTEGER, "intValue", MethodTypeDesc.of(ConstantDescs.CD_int));
							maskU16();
						}
						case I32, U32 ->
							cb.invokevirtual(CD_BIG_INTEGER, "intValue", MethodTypeDesc.of(ConstantDescs.CD_int));
						case I64, U64 ->
							cb.invokevirtual(CD_BIG_INTEGER, "longValue", MethodTypeDesc.of(ConstantDescs.CD_long));
					}
				}
				case I8 -> {
					switch (destType) {
						case INT -> intToBigInteger();
						case I8 -> {
						}
						case U8 -> maskU8();
						case I16 -> {
						}
						case U16 -> maskU16();
						case I32 -> {
						}
						case U32 -> {
						}
						case I64 -> cb.i2l();
						case U64 -> cb.i2l();
					}
				}
				case U8 -> {
					switch (destType) {
						case INT -> intToBigInteger();
						case I8 -> cb.i2b();
						case U8 -> {
						}
						case I16 -> {
						}
						case U16 -> {
						}
						case I32 -> {
						}
						case U32 -> {
						}
						case I64 -> cb.i2l();
						case U64 -> cb.i2l();
					}
				}
				case I16 -> {
					switch (destType) {
						case INT -> intToBigInteger();
						case I8 -> cb.i2b();
						case U8 -> maskU8();
						case I16 -> {
						}
						case U16 -> maskU16();
						case I32 -> {
						}
						case U32 -> {
						}
						case I64 -> cb.i2l();
						case U64 -> cb.i2l();
					}
				}
				case U16 -> {
					switch (destType) {
						case INT -> intToBigInteger();
						case I8 -> cb.i2b();
						case U8 -> maskU8();
						case I16 -> cb.i2s();
						case U16 -> {
						}
						case I32 -> {
						}
						case U32 -> {
						}
						case I64 -> cb.i2l();
						case U64 -> cb.i2l();
					}
				}
				case I32 -> {
					switch (destType) {
						case INT -> intToBigInteger();
						case I8 -> cb.i2b();
						case U8 -> maskU8();
						case I16 -> cb.i2s();
						case U16 -> maskU16();
						case I32 -> {
						}
						case U32 -> {
						}
						case I64 -> cb.i2l();
						case U64 -> cb.i2l();
					}
				}
				case U32 -> {
					switch (destType) {
						case INT -> unsignedIntToBigInteger();
						case I8 -> cb.i2b();
						case U8 -> maskU8();
						case I16 -> cb.i2s();
						case U16 -> maskU16();
						case I32 -> {
						}
						case U32 -> {
						}
						case I64 -> unsignedIntToLong();
						case U64 -> unsignedIntToLong();
					}
				}
				case I64 -> {
					switch (destType) {
						case INT -> longToBigInteger();
						case I8 -> {
							cb.l2i();
							cb.i2b();
						}
						case U8 -> {
							cb.l2i();
							maskU8();
						}
						case I16 -> {
							cb.l2i();
							cb.i2s();
						}
						case U16 -> {
							cb.l2i();
							maskU16();
						}
						case I32 -> cb.l2i();
						case U32 -> cb.l2i();
						case I64 -> {
						}
						case U64 -> {
						}
					}
				}
				case U64 -> {
					switch (destType) {
						case INT -> unsignedLongToBigInteger();
						case I8 -> {
							cb.l2i();
							cb.i2b();
						}
						case U8 -> {
							cb.l2i();
							maskU8();
						}
						case I16 -> {
							cb.l2i();
							cb.i2s();
						}
						case U16 -> {
							cb.l2i();
							maskU16();
						}
						case I32 -> cb.l2i();
						case U32 -> cb.l2i();
						case I64 -> {
						}
						case U64 -> {
						}
					}
				}
			}
			storeRegister(dest);
		}

		private void emitIntegerShift(RegisterId dest, RegisterId lhs, RegisterId rhs, IntegerType integerType,
			boolean left) {
			switch (integerType) {
				case INT -> {
					loadRegister(lhs);
					loadBigIntegerAsInt(rhs);
					cb.invokevirtual(CD_BIG_INTEGER, left ? "shiftLeft" : "shiftRight",
						MethodTypeDesc.of(CD_BIG_INTEGER, ConstantDescs.CD_int));
					storeRegister(dest);
				}
				case I8 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.sipush(8);
					cb.if_icmpge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					if(left) {
						cb.ishl();
						cb.i2b();
					}
					else {
						cb.ishr();
					}
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.iconst_0();
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case U8 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.sipush(8);
					cb.if_icmpge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					if(left) {
						cb.ishl();
					}
					else {
						cb.iushr();
					}
					maskU8();
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.iconst_0();
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case I16 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.sipush(16);
					cb.if_icmpge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					if(left) {
						cb.ishl();
						cb.i2s();
					}
					else {
						cb.ishr();
					}
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.iconst_0();
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case U16 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.sipush(16);
					cb.if_icmpge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					if(left) {
						cb.ishl();
					}
					else {
						cb.iushr();
					}
					maskU16();
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.iconst_0();
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case I32 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.sipush(32);
					cb.if_icmpge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					if(left) {
						cb.ishl();
					}
					else {
						cb.ishr();
					}
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.iconst_0();
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case U32 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.sipush(32);
					cb.if_icmpge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					if(left) {
						cb.ishl();
					}
					else {
						cb.iushr();
					}
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.iconst_0();
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case I64 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.ldc(64L);
					cb.lcmp();
					cb.ifge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					cb.l2i();
					if(left) {
						cb.lshl();
					}
					else {
						cb.lshr();
					}
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.ldc(0L);
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
				case U64 -> {
					var zeroLabel = cb.newLabel();
					var endLabel = cb.newLabel();

					loadRegister(rhs);
					cb.ldc(64L);
					cb.lcmp();
					cb.ifge(zeroLabel);

					loadRegister(lhs);
					loadRegister(rhs);
					cb.l2i();
					if(left) {
						cb.lshl();
					}
					else {
						cb.lushr();
					}
					cb.goto_(endLabel);

					cb.labelBinding(zeroLabel);
					cb.ldc(0L);
					cb.labelBinding(endLabel);
					storeRegister(dest);
				}
			}
		}

		private void emitIntegerCompare(RegisterId dest, RegisterId lhs, RegisterId rhs, IntegerType integerType,
			Consumer<Label> bigIntegerBranch, Consumer<Label> u8Branch) {
			switch (integerType) {
				case INT -> {
					loadRegister(lhs);
					loadRegister(rhs);
					cb.invokevirtual(CD_BIG_INTEGER, "compareTo",
						MethodTypeDesc.of(ConstantDescs.CD_int, CD_BIG_INTEGER));
					emitBooleanFromBranch(bigIntegerBranch);
					storeRegister(dest);
				}
				case I8 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					emitBooleanFromBranch(u8Branch);
					storeRegister(dest);
				}
				case U8 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					emitBooleanFromBranch(u8Branch);
					storeRegister(dest);
				}
				case I16 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					emitBooleanFromBranch(u8Branch);
					storeRegister(dest);
				}
				case U16 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					emitBooleanFromBranch(u8Branch);
					storeRegister(dest);
				}
				case I32 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					emitBooleanFromBranch(u8Branch);
					storeRegister(dest);
				}
				case U32 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					cb.invokestatic(ConstantDescs.CD_Integer, "compareUnsigned",
						MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_int, ConstantDescs.CD_int));
					emitBooleanFromBranch(bigIntegerBranch);
					storeRegister(dest);
				}
				case I64 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					cb.lcmp();
					emitBooleanFromBranch(bigIntegerBranch);
					storeRegister(dest);
				}
				case U64 -> {
					loadRegister(lhs);
					loadRegister(rhs);
					cb.invokestatic(ConstantDescs.CD_Long, "compareUnsigned",
						MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_long, ConstantDescs.CD_long));
					emitBooleanFromBranch(bigIntegerBranch);
					storeRegister(dest);
				}
			}
		}

		private void loadBigIntegerAsInt(RegisterId register) {
			loadRegister(register);
			cb.invokevirtual(CD_BIG_INTEGER, "intValueExact", MethodTypeDesc.of(ConstantDescs.CD_int));
		}

		private void intToBigInteger() {
			cb.i2l().invokestatic(CD_BIG_INTEGER, "valueOf", MethodTypeDesc.of(CD_BIG_INTEGER, ConstantDescs.CD_long));
		}

		private void unsignedIntToLong() {
			cb.invokestatic(ConstantDescs.CD_Integer, "toUnsignedLong",
				MethodTypeDesc.of(ConstantDescs.CD_long, ConstantDescs.CD_int));
		}

		private void longToBigInteger() {
			cb.invokestatic(CD_BIG_INTEGER, "valueOf", MethodTypeDesc.of(CD_BIG_INTEGER, ConstantDescs.CD_long));
		}

		private void unsignedIntToBigInteger() {
			cb.invokestatic(ConstantDescs.CD_Integer, "toUnsignedLong",
				MethodTypeDesc.of(ConstantDescs.CD_long, ConstantDescs.CD_int))
				.invokestatic(CD_BIG_INTEGER, "valueOf", MethodTypeDesc.of(CD_BIG_INTEGER, ConstantDescs.CD_long));
		}

		private void unsignedLongToBigInteger() {
			var stringSlot = cb.allocateLocal(TypeKind.REFERENCE);
			cb.invokestatic(ConstantDescs.CD_Long, "toUnsignedString",
				MethodTypeDesc.of(ConstantDescs.CD_String, ConstantDescs.CD_long))
				.storeLocal(TypeKind.REFERENCE, stringSlot).new_(CD_BIG_INTEGER).dup()
				.loadLocal(TypeKind.REFERENCE, stringSlot).invokespecial(CD_BIG_INTEGER, ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String));
		}

		private void maskU8() {
			cb.sipush(0xFF);
			cb.iand();
		}

		private void maskU16() {
			cb.ldc(0xFFFF);
			cb.iand();
		}

		private void emitBooleanFromBranch(Consumer<Label> branch) {
			var trueLabel = cb.newLabel();
			var endLabel = cb.newLabel();

			branch.accept(trueLabel);
			cb.iconst_0().goto_(endLabel).labelBinding(trueLabel).iconst_1().labelBinding(endLabel);
		}

		private TypeKind arrayElementKind(Token elementType) {
			return TypeKind.from(tokenAsClassDesc(elementType));
		}

		private void emitFinally(Region.Finally finallyRegion) {
			var start = cb.newLabel();
			var end = cb.newLabel();
			var handler = cb.newLabel();
			var done = cb.newLabel();
			var exceptionSlot = cb.allocateLocal(TypeKind.REFERENCE);
			var originalBlocks = new HashMap<>(blocks);
			var finallyJumpLabels = new LinkedHashMap<FinallyJumpTarget, Label>();
			for(var target : scanFinallyJumpTargets(finallyRegion.action(), originalBlocks)) {
				finallyJumpLabels.put(target, cb.newLabel());
			}

			var oldReturnMode = returnMode;
			Label returnFinallyLabel = null;
			if(containsReturn(finallyRegion.action())) {
				returnFinallyLabel = cb.newLabel();
				returnMode = new ReturnMode.Branch(returnFinallyLabel);
			}

			cb.labelBinding(start);
			installFinallyJumpLabels(originalBlocks, finallyJumpLabels);
			emitRegion(finallyRegion.action());
			blocks.clear();
			blocks.putAll(originalBlocks);
			returnMode = oldReturnMode;
			cb.labelBinding(end);

			if(reachable) {
				emitRegion(finallyRegion.ensuring());
				if(reachable) {
					cb.goto_(done);
				}
			}

			boolean oldReachable = reachable;

			for(var entry : finallyJumpLabels.entrySet()) {
				reachable = true;
				cb.labelBinding(entry.getValue());
				emitRegion(finallyRegion.ensuring());
				if(reachable) {
					cb.goto_(entry.getKey().label(originalBlocks));
				}
			}

			if(returnFinallyLabel != null) {
				reachable = true;
				cb.labelBinding(returnFinallyLabel);
				emitRegion(finallyRegion.ensuring());

				switch (returnMode) {
					case ReturnMode.Direct() -> {
						cb.loadLocal(returnKind, getReturnValueSlot());
						emitReturnFromStack();
					}

					case ReturnMode.Branch(var label) -> {
						cb.goto_(label);
					}
				}
			}

			reachable = true;

			cb.labelBinding(handler);
			cb.storeLocal(TypeKind.REFERENCE, exceptionSlot);
			emitRegion(finallyRegion.ensuring());
			if(reachable) {
				cb.loadLocal(TypeKind.REFERENCE, exceptionSlot);
				cb.athrow();
			}

			cb.labelBinding(done);
			cb.exceptionCatchAll(start, end, handler);

			reachable = oldReachable;
		}

		private Set<FinallyJumpTarget> scanFinallyJumpTargets(Region region, Map<BlockId, BlockLabels> knownBlocks) {
			var targets = new LinkedHashSet<FinallyJumpTarget>();
			new InstructionScanner() {
				@Override
				protected void visitInstruction(Instruction instruction) {
					switch (instruction) {
						case Instruction.BlockBreak blockBreak ->
							addFinallyJumpTarget(blockBreak.blockId(), FinallyJumpTargetKind.END);

						case Instruction.BlockBreakIf blockBreakIf ->
							addFinallyJumpTarget(blockBreakIf.blockId(), FinallyJumpTargetKind.END);

						case Instruction.BlockBreakUnless blockBreakUnless ->
							addFinallyJumpTarget(blockBreakUnless.blockId(), FinallyJumpTargetKind.END);

						case Instruction.BlockRetry blockRetry ->
							addFinallyJumpTarget(blockRetry.blockId(), FinallyJumpTargetKind.START);

						case Instruction.IsEnumVariantOrBreak isEnumVariantOrBreak ->
							addFinallyJumpTarget(isEnumVariantOrBreak.notVariantBlockId(), FinallyJumpTargetKind.END);

						default -> {
						}
					}
				}

				private void addFinallyJumpTarget(BlockId blockId, FinallyJumpTargetKind kind) {
					if(knownBlocks.containsKey(blockId)) {
						targets.add(new FinallyJumpTarget(blockId, kind));
					}
				}
			}.scan(region);

			return targets;
		}

		private boolean containsReturn(Region region) {
			var scanner = new InstructionScanner() {
				boolean containsReturn = false;

				@Override
				protected void visitInstruction(Instruction instruction) {
					if(instruction instanceof Instruction.Return) {
						containsReturn = true;
					}
				}
			};
			scanner.scan(region);

			return scanner.containsReturn;
		}

		private void installFinallyJumpLabels(Map<BlockId, BlockLabels> originalBlocks,
			Map<FinallyJumpTarget, Label> finallyJumpLabels) {
			for(var entry : originalBlocks.entrySet()) {
				var blockId = entry.getKey();
				var labels = entry.getValue();
				var start = finallyJumpLabels.getOrDefault(new FinallyJumpTarget(blockId, FinallyJumpTargetKind.START),
					labels.start);
				var end = finallyJumpLabels.getOrDefault(new FinallyJumpTarget(blockId, FinallyJumpTargetKind.END),
					labels.end);

				blocks.put(blockId, new BlockLabels(start, end));
			}
		}

		private void emitReturn(Instruction.Return ret) {
			switch (returnMode) {
				case ReturnMode.Direct() -> {
					loadRegister(ret.src());
					emitReturnFromStack();
					reachable = false;
				}

				case ReturnMode.Branch(var label) -> {
					loadRegister(ret.src());
					cb.storeLocal(returnKind, getReturnValueSlot());
					cb.goto_(label);
					reachable = false;
				}
			}
		}

		private void boxValue(Token token) {
			var kind = TypeKind.from(tokenAsClassDesc(token));
			switch (kind) {
				case BOOLEAN -> cb.invokestatic(ConstantDescs.CD_Boolean, "valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Boolean, ConstantDescs.CD_boolean));

				case BYTE -> {
					if(isI8Token(token) || isU8Token(token)) {
						cb.i2b();
					}
					cb.invokestatic(ConstantDescs.CD_Byte, "valueOf",
						MethodTypeDesc.of(ConstantDescs.CD_Byte, ConstantDescs.CD_byte));
				}

				case CHAR -> cb.invokestatic(ConstantDescs.CD_Character, "valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Character, ConstantDescs.CD_char));

				case SHORT -> {
					if(isI16Token(token) || isU16Token(token)) {
						cb.i2s();
					}
					cb.invokestatic(ConstantDescs.CD_Short, "valueOf",
						MethodTypeDesc.of(ConstantDescs.CD_Short, ConstantDescs.CD_short));
				}

				case INT -> cb.invokestatic(ConstantDescs.CD_Integer, "valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Integer, ConstantDescs.CD_int));

				case LONG -> cb.invokestatic(ConstantDescs.CD_Long, "valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Long, ConstantDescs.CD_long));

				case FLOAT -> cb.invokestatic(ConstantDescs.CD_Float, "valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Float, ConstantDescs.CD_float));

				case DOUBLE -> cb.invokestatic(ConstantDescs.CD_Double, "valueOf",
					MethodTypeDesc.of(ConstantDescs.CD_Double, ConstantDescs.CD_double));

				case REFERENCE -> {
				}

				case VOID -> throw new IllegalArgumentException("Cannot box void");
			}
		}

		private void unboxValue(Token token) {
			var type = tokenAsClassDesc(token);
			switch (TypeKind.from(type)) {
				case BOOLEAN -> cb.checkcast(ConstantDescs.CD_Boolean).invokevirtual(ConstantDescs.CD_Boolean,
					"booleanValue", MethodTypeDesc.of(ConstantDescs.CD_boolean));

				case BYTE -> {
					cb.checkcast(ConstantDescs.CD_Byte).invokevirtual(ConstantDescs.CD_Byte, "byteValue",
						MethodTypeDesc.of(ConstantDescs.CD_byte));
					if(isU8Token(token)) {
						cb.invokestatic(ConstantDescs.CD_Byte, "toUnsignedInt",
							MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_byte));
					}
				}

				case CHAR -> cb.checkcast(ConstantDescs.CD_Character).invokevirtual(ConstantDescs.CD_Character,
					"charValue", MethodTypeDesc.of(ConstantDescs.CD_char));

				case SHORT -> {
					cb.checkcast(ConstantDescs.CD_Short).invokevirtual(ConstantDescs.CD_Short, "shortValue",
						MethodTypeDesc.of(ConstantDescs.CD_short));
					if(isU16Token(token)) {
						cb.invokestatic(ConstantDescs.CD_Short, "toUnsignedInt",
							MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_short));
					}
				}

				case INT -> cb.checkcast(ConstantDescs.CD_Integer).invokevirtual(ConstantDescs.CD_Integer, "intValue",
					MethodTypeDesc.of(ConstantDescs.CD_int));

				case LONG -> cb.checkcast(ConstantDescs.CD_Long).invokevirtual(ConstantDescs.CD_Long, "longValue",
					MethodTypeDesc.of(ConstantDescs.CD_long));

				case FLOAT -> cb.checkcast(ConstantDescs.CD_Float).invokevirtual(ConstantDescs.CD_Float, "floatValue",
					MethodTypeDesc.of(ConstantDescs.CD_float));

				case DOUBLE -> cb.checkcast(ConstantDescs.CD_Double).invokevirtual(ConstantDescs.CD_Double,
					"doubleValue", MethodTypeDesc.of(ConstantDescs.CD_double));

				case REFERENCE -> cb.checkcast(type);

				case VOID -> throw new IllegalArgumentException("Cannot unbox void");
			}
		}

		private void unboxValueFrom(ClassDesc src, Token dest) {
			var destDesc = tokenAsClassDesc(dest);
			if(src.equals(ConstantDescs.CD_Object) && !destDesc.equals(ConstantDescs.CD_Object)) {
				unboxValue(dest);
			}
		}

		private boolean isU8Token(Token token) {
			return token instanceof Token.Builtin(var builtinType)
				&& builtinType instanceof BuiltinType.Int(var integerType) && integerType == IntegerType.U8;
		}

		private boolean isI8Token(Token token) {
			return token instanceof Token.Builtin(var builtinType)
				&& builtinType instanceof BuiltinType.Int(var integerType) && integerType == IntegerType.I8;
		}

		private boolean isU16Token(Token token) {
			return token instanceof Token.Builtin(var builtinType)
				&& builtinType instanceof BuiltinType.Int(var integerType) && integerType == IntegerType.U16;
		}

		private boolean isI16Token(Token token) {
			return token instanceof Token.Builtin(var builtinType)
				&& builtinType instanceof BuiltinType.Int(var integerType) && integerType == IntegerType.I16;
		}

		private void emitRuntimeUnsupported(String message) {
			cb.new_(CD_UNSUPPORTED_OPERATION_EXCEPTION).dup().ldc(message)
				.invokespecial(CD_UNSUPPORTED_OPERATION_EXCEPTION, ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String))
				.athrow();
		}

		private int getReturnValueSlot() {
			if(returnValueSlot == null) {
				returnValueSlot = cb.allocateLocal(returnKind);
			}

			return returnValueSlot;
		}

		private void emitReturnFromStack() {
			boxValue(returnToken);
			cb.invokestatic(CD_TRAMPOLINE_RESULT, "of",
				MethodTypeDesc.of(CD_TRAMPOLINE_RESULT, ConstantDescs.CD_Object)).areturn();
		}

		private interface ParentTokenParameterLoader {
			void load(int index);
		}

		private enum PartialApplicationKind {
			FUNCTION, TOKEN, ERASED,
		}

		private static final class BlockLabels {
			private final Label start;
			private final Label end;
			private boolean endReachable = false;

			private BlockLabels(Label start, Label end) {
				this.start = start;
				this.end = end;
			}
		}

		private record FinallyJumpTarget(BlockId blockId, FinallyJumpTargetKind kind) {
			private Label label(Map<BlockId, BlockLabels> blocks) {
				var labels = blocks.get(blockId);
				Objects.requireNonNull(labels, "Block labels not found");

				return switch (kind) {
					case START -> labels.start;
					case END -> labels.end;
				};
			}
		}

		private enum FinallyJumpTargetKind {
			START, END
		}

		private sealed interface ReturnMode {
			record Direct() implements ReturnMode {
			}
			record Branch(Label branch) implements ReturnMode {
			}
		}
	}
}
