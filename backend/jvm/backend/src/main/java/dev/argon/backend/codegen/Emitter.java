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
import java.lang.classfile.attribute.InnerClassesAttribute;
import java.lang.classfile.attribute.InnerClassInfo;
import java.lang.classfile.attribute.ModuleAttribute;
import java.lang.classfile.attribute.PermittedSubclassesAttribute;
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
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

final class Emitter {
	private static final ClassFile CLASS_FILE = ClassFile.of();
	private static final String MODULE_INFO_ENTRY = "module-info.class";
	private static final ClassDesc CD_LAMBDA_METAFACTORY = ClassDesc.of("java.lang.invoke.LambdaMetafactory");
	private static final ClassDesc CD_PARTIAL_APPLICATION_SUPPORT = ClassDesc.of("dev.argon.runtime.PartialApplicationSupport");
	private static final ClassDesc CD_STRING_CONCAT_FACTORY = ClassDesc.of("java.lang.invoke.StringConcatFactory");
	private static final DirectMethodHandleDesc BSM_LAMBDA_METAFACTORY = MethodHandleDesc.ofMethod(
		DirectMethodHandleDesc.Kind.STATIC,
		CD_LAMBDA_METAFACTORY,
		"metafactory",
		MethodTypeDesc.of(
			ConstantDescs.CD_CallSite,
			ConstantDescs.CD_MethodHandles_Lookup,
			ConstantDescs.CD_String,
			ConstantDescs.CD_MethodType,
			ConstantDescs.CD_MethodType,
			ConstantDescs.CD_MethodHandle,
			ConstantDescs.CD_MethodType
		)
	);
	private static final DirectMethodHandleDesc BSM_PARTIAL_TOKEN_FUNCTION = MethodHandleDesc.ofMethod(
		DirectMethodHandleDesc.Kind.STATIC,
		CD_PARTIAL_APPLICATION_SUPPORT,
		"tokenFunction",
		MethodTypeDesc.of(
			ConstantDescs.CD_CallSite,
			ConstantDescs.CD_MethodHandles_Lookup,
			ConstantDescs.CD_String,
			ConstantDescs.CD_MethodType,
			ConstantDescs.CD_MethodHandle,
			ConstantDescs.CD_int
		)
	);
	private static final DirectMethodHandleDesc BSM_STRING_CONCAT_WITH_CONSTANTS = MethodHandleDesc.ofMethod(
		DirectMethodHandleDesc.Kind.STATIC,
		CD_STRING_CONCAT_FACTORY,
		"makeConcatWithConstants",
		MethodTypeDesc.of(
			ConstantDescs.CD_CallSite,
			ConstantDescs.CD_MethodHandles_Lookup,
			ConstantDescs.CD_String,
			ConstantDescs.CD_MethodType,
			ConstantDescs.CD_String
		)
	);

	public Emitter(ProgramModel program, ZipOutputStream zos) {
		this.program = program;
		this.zos = zos;
	}

	private final ProgramModel program;
	private final ZipOutputStream zos;
	private boolean emittedModuleInfo = false;

	public void emit() throws IOException {
		emitClassFiles();

		emitModuleInfo();

		for(var module : program.modules()) {
			emitModule(module);
		}
	}

	private void emitClassFiles() throws IOException {
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

		emitFunctionImplementationBody(
			cb,
			function.signature(),
			implementation.get(),
			Optional.empty(),
			index -> {
				throw new UnsupportedOperationException("Function cannot load parent token parameter");
			}
		);
	}

	private void emitFunctionImplementationBody(
		CodeBuilder cb,
		FunctionSignature signature,
		FunctionImplementation implementation,
		Optional<ClassDesc> receiverType,
		BlockEmitter.ParentTokenParameterLoader parentTokenParameterLoader
	) {
		switch(implementation) {
			case FunctionImplementation.Extern extern -> emitExternFunctionBody(cb, signature, extern);
			case FunctionImplementation.VmIr ir -> {
				var registerOffset = 0;
				int[] tokenSlots = new int[signature.tokenParameters().size()];
				ClassDesc[] tokenTypes = new ClassDesc[tokenSlots.length];
				TypeKind[] tokenKinds = new TypeKind[tokenSlots.length];
				int[] registerSlots = new int[Math.addExact(
					Math.addExact(receiverType.isPresent() ? 1 : 0, signature.parameters().size()),
					ir.body().variables().variables().size()
				)];
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
					tokenTypes[i] = tokenType;
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

				var returnKind = TypeKind.from(tokenAsClassDesc(signature.returnType()));

				var blockEmitter = new BlockEmitter(
					cb,
					tokenSlots,
					tokenTypes,
					tokenKinds,
					parentTokenParameterLoader,
					registerSlots,
					registerTokens,
					registerTypes,
					registerKinds,
					returnKind
				);
				blockEmitter.emitBlock(ir.body().block());
			}
		}
	}

	private void emitExternFunctionBody(
		CodeBuilder cb,
		FunctionSignature signature,
		FunctionImplementation.Extern extern
	) {
		var jvmFunction = decodeJvmFunction(extern);
		var externMethod = resolveExternMethod(jvmFunction);
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

	private MethodTypeDesc functionSignatureMethodType(FunctionSignature signature) {
		var parameterTypes = new ArrayList<ClassDesc>();

		parameterTypes.addAll(tokenParameterDescs(signature));
		for(var parameter : signature.parameters()) {
			parameterTypes.add(tokenAsClassDesc(parameter.paramType()));
		}

		return MethodTypeDesc.of(ClassDesc.of("dev.argon.runtime.Trampoline"), parameterTypes);
	}

	private List<ClassDesc> functionParameterTypes(FunctionDefinition function) {
		var parameterTypes = new ArrayList<ClassDesc>();

		parameterTypes.addAll(tokenParameterDescs(function.signature()));
		for(var parameter : function.signature().parameters()) {
			parameterTypes.add(tokenAsClassDesc(parameter.paramType()));
		}

		return parameterTypes;
	}

	private List<ClassDesc> tokenParameterDescs(FunctionSignature signature) {
		return signature.tokenParameters().stream()
			.map(parameter -> tokenAsClassDesc(parameter.kind()))
			.toList();
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
		var tokenParameterCount = definition.signature().tokenParameters().size();
		var fields = definition.fields();
		var fieldDescs = fields.stream()
			.map(field -> tokenAsClassDesc(field.fieldType()))
			.toList();
		var tokenParameterDescs = tokenParameterDescs(definition.signature());
		var constructorDescs = new ArrayList<ClassDesc>(tokenParameterDescs);
		constructorDescs.addAll(fieldDescs);

		var bytes = CLASS_FILE.build(classDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object);

			for(int i = 0; i < tokenParameterCount; ++i) {
				classBuilder.withField(recordTokenParameterFieldName(i), tokenParameterDescs.get(i), ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

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
				MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs),
				ClassFile.ACC_PUBLIC,
				codeBuilder -> {
					codeBuilder
						.aload(0)
						.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < tokenParameterCount; ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder
							.aload(0)
							.loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(), localSlot)
							.putfield(classDesc, recordTokenParameterFieldName(i), tokenParameterDesc);
						localSlot += TypeKind.from(tokenParameterDesc).asLoadable().slotSize();
					}

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

	private void emitEnum(TubeFileEntry.EnumDefinition enumDef) throws IOException {
		var definition = enumDef.definition();
		var classDesc = ClassNaming.typeDefinitionClassDescriptor(program, definition._import());
		var tokenParameterCount = definition.signature().tokenParameters().size();
		var tokenParameterDescs = tokenParameterDescs(definition.signature());
		var variantDescs = definition.variants().stream()
			.map(variant -> enumVariantClassDesc(classDesc, variant))
			.toList();

		var baseBytes = CLASS_FILE.build(classDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_ABSTRACT | ClassFile.ACC_SUPER)
				.withSuperclass(ConstantDescs.CD_Object)
				.with(PermittedSubclassesAttribute.ofSymbols(variantDescs))
				.with(InnerClassesAttribute.of(definition.variants().stream()
					.map(variant -> InnerClassInfo.of(
						enumVariantClassDesc(classDesc, variant),
						Optional.of(classDesc),
						Optional.of(ClassNaming.fieldName(variant.name())),
						ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL
					))
					.toList()
				));

			for(int i = 0; i < tokenParameterCount; ++i) {
				classBuilder.withField(recordTokenParameterFieldName(i), tokenParameterDescs.get(i), ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			classBuilder.withMethodBody(
				ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, tokenParameterDescs.toArray(ClassDesc[]::new)),
				ClassFile.ACC_PROTECTED,
				codeBuilder -> {
					codeBuilder
						.aload(0)
						.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < tokenParameterCount; ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder
							.aload(0)
							.loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(), localSlot)
							.putfield(classDesc, recordTokenParameterFieldName(i), tokenParameterDesc);
						localSlot += TypeKind.from(tokenParameterDesc).asLoadable().slotSize();
					}

					codeBuilder.return_();
				}
			);
		});
		writeEntry(classEntryName(classDesc), baseBytes);

		for(var variant : definition.variants()) {
			emitEnumVariant(classDesc, tokenParameterDescs, variant);
		}
	}

	private void emitEnumVariant(ClassDesc enumClassDesc, List<ClassDesc> enumTokenParameterDescs, EnumVariantDefinition variant) throws IOException {
		var variantDesc = enumVariantClassDesc(enumClassDesc, variant);
		var argDescs = enumVariantArgDescs(variant);
		var fieldDescs = variant.fields().stream()
			.map(field -> tokenAsClassDesc(field.fieldType()))
			.toList();
		var constructorDescs = new ArrayList<ClassDesc>(enumTokenParameterDescs);
		constructorDescs.addAll(argDescs);
		constructorDescs.addAll(fieldDescs);

		var bytes = CLASS_FILE.build(variantDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(enumClassDesc)
				.with(InnerClassesAttribute.of(InnerClassInfo.of(
					variantDesc,
					Optional.of(enumClassDesc),
					Optional.of(ClassNaming.fieldName(variant.name())),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC | ClassFile.ACC_FINAL
				)));

			for(int i = 0; i < argDescs.size(); ++i) {
				classBuilder.withField(":pv" + i, argDescs.get(i), ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(int i = 0; i < variant.fields().size(); ++i) {
				var field = variant.fields().get(i);
				var flags = ClassFile.ACC_PUBLIC;
				if(!field.mutable()) {
					flags |= ClassFile.ACC_FINAL;
				}
				classBuilder.withField(ClassNaming.fieldName(field.name()), fieldDescs.get(i), flags);
			}

			classBuilder.withMethodBody(
				ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs),
				ClassFile.ACC_PUBLIC,
					codeBuilder -> {
						codeBuilder
							.aload(0);

						var localSlot = 1;
						for(int i = 0; i < enumTokenParameterDescs.size(); ++i) {
							var tokenParameterDesc = enumTokenParameterDescs.get(i);
							codeBuilder.loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(), localSlot);
							localSlot += TypeKind.from(tokenParameterDesc).asLoadable().slotSize();
						}
						codeBuilder.invokespecial(
							enumClassDesc,
							ConstantDescs.INIT_NAME,
							MethodTypeDesc.of(ConstantDescs.CD_void, enumTokenParameterDescs.toArray(ClassDesc[]::new))
						);

						for(int i = 0; i < argDescs.size(); ++i) {
						var argDesc = argDescs.get(i);
						var argKind = TypeKind.from(argDesc).asLoadable();
						codeBuilder
							.aload(0)
							.loadLocal(argKind, localSlot)
							.putfield(variantDesc, ":pv" + i, argDesc);
						localSlot += argKind.slotSize();
					}

					for(int i = 0; i < variant.fields().size(); ++i) {
						var field = variant.fields().get(i);
						var fieldDesc = fieldDescs.get(i);
						var fieldKind = TypeKind.from(fieldDesc).asLoadable();
						codeBuilder
							.aload(0)
							.loadLocal(fieldKind, localSlot)
							.putfield(variantDesc, ClassNaming.fieldName(field.name()), fieldDesc);
						localSlot += fieldKind.slotSize();
					}

					codeBuilder.return_();
				}
			);
		});

		writeEntry(classEntryName(variantDesc), bytes);
	}

	private ClassDesc enumVariantClassDesc(ClassDesc enumClassDesc, EnumVariantDefinition variant) {
		return enumClassDesc.nested(ClassNaming.variantName(variant.name()));
	}

	private ClassDesc enumVariantClassDesc(ClassDesc enumClassDesc, Identifier variantName) {
		return enumClassDesc.nested(ClassNaming.variantName(variantName));
	}

	private RecordDefinition recordDefinition(UnsignedBigInteger recordId) {
		for(var module : program.modules()) {
			for(var export : module.exports()) {
				if(export instanceof ProgramModel.ModuleExportEntry.RecordDefinition(var recordDef)) {
					var definition = recordDef.definition();
					if(definition.recordId().equals(recordId)) {
						return definition;
					}
				}
			}
		}

		throw new IllegalArgumentException("Could not find record definition: " + recordId);
	}

	private EnumDefinition enumDefinition(UnsignedBigInteger enumId) {
		for(var module : program.modules()) {
			for(var export : module.exports()) {
				if(export instanceof ProgramModel.ModuleExportEntry.EnumDefinition(var enumDef)) {
					var definition = enumDef.definition();
					if(definition.enumId().equals(enumId)) {
						return definition;
					}
				}
			}
		}

		throw new IllegalArgumentException("Could not find enum definition: " + enumId);
	}

	private EnumVariantDefinition enumVariantDefinition(UnsignedBigInteger variantId) {
		var variantInfo = program.getEnumVariantInfo(variantId);
		return enumDefinition(variantInfo.enumId()).variants().stream()
			.filter(variant -> variant.name().equals(variantInfo.name()))
			.findAny()
			.orElseThrow(() -> new IllegalArgumentException("Could not find enum variant definition: " + variantId));
	}

	private FunctionDefinition functionDefinition(UnsignedBigInteger functionId) {
		for(var module : program.modules()) {
			for(var export : module.exports()) {
				if(export instanceof ProgramModel.ModuleExportEntry.FunctionDefinition(var functionDef)) {
					var definition = functionDef.definition();
					if(definition.functionId().equals(functionId)) {
						return definition;
					}
				}
			}
		}

		throw new IllegalArgumentException("Could not find function definition: " + functionId);
	}

	private InstanceDefinition instanceDefinition(UnsignedBigInteger instanceId) {
		for(var module : program.modules()) {
			for(var export : module.exports()) {
				if(export instanceof ProgramModel.ModuleExportEntry.InstanceDefinition(var instanceDef)) {
					var definition = instanceDef.definition();
					if(definition.instanceId().equals(instanceId)) {
						return definition;
					}
				}
			}
		}

		throw new IllegalArgumentException("Could not find instance definition: " + instanceId);
	}

	private List<ClassDesc> enumVariantArgDescs(EnumVariantDefinition variant) {
		var argDescs = new ArrayList<ClassDesc>();
		argDescs.addAll(tokenParameterDescs(variant.signature()));
		for(var parameter : variant.signature().parameters()) {
			argDescs.add(tokenAsClassDesc(parameter.paramType()));
		}

		return argDescs;
	}

	private String recordTokenParameterFieldName(int index) {
		return ":pt" + index;
	}

	private void emitTrait(TubeFileEntry.TraitDefinition traitDef) throws IOException {
		var definition = traitDef.definition();
		var classDesc = ClassNaming.typeDefinitionClassDescriptor(program, definition._import());

		var bytes = CLASS_FILE.build(classDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_INTERFACE | ClassFile.ACC_ABSTRACT)
				.withSuperclass(ConstantDescs.CD_Object);

			var tokenParameterDescs = tokenParameterDescs(definition.signature());
			for(int i = 0; i < definition.signature().tokenParameters().size(); ++i) {
				classBuilder.withMethod(
					recordTokenParameterFieldName(i),
					MethodTypeDesc.of(tokenParameterDescs.get(i)),
					ClassFile.ACC_PUBLIC | ClassFile.ACC_ABSTRACT,
					methodBuilder -> {
					}
				);
			}

			for(var method : definition.methods()) {
				var methodName = ClassNaming.methodName(method.name(), method.erasedSignature());
				var methodType = functionSignatureMethodType(method.signature());

				if(method._abstract()) {
					classBuilder.withMethod(
						methodName,
						methodType,
						ClassFile.ACC_PUBLIC | ClassFile.ACC_ABSTRACT,
						methodBuilder -> {
						}
					);
				}
				else {
					var implementation = method.implementation()
						.orElseThrow(() -> new RuntimeException("Trait method implementation is missing"));
					classBuilder.withMethodBody(
						methodName,
						methodType,
						ClassFile.ACC_PUBLIC,
						codeBuilder -> emitFunctionImplementationBody(
							codeBuilder,
							method.signature(),
							implementation,
							Optional.of(classDesc),
							index -> codeBuilder
								.aload(codeBuilder.receiverSlot())
								.invokeinterface(
									classDesc,
									recordTokenParameterFieldName(index),
									MethodTypeDesc.of(tokenParameterDescs.get(index))
								)
						)
					);
				}
			}
		});

		writeEntry(classEntryName(classDesc), bytes);
	}

	private void emitInstance(TubeFileEntry.InstanceDefinition instanceDef) throws IOException {
		var definition = instanceDef.definition();
		var classDesc = ClassNaming.typeDefinitionClassDescriptor(program, definition._import());
		var traitDesc = switch(definition.signature().returnType()) {
			case Token.Trait trait -> tokenAsClassDesc(trait);
			default -> throw new IllegalArgumentException("Instance definition return type is not a trait");
		};
		var superclassDesc = switch(definition.signature().returnType()) {
			case Token.Trait trait when isCoreExceptionTrait(trait) -> ClassDesc.of("dev.argon.runtime.ArgonException");
			default -> ConstantDescs.CD_Object;
		};
		var tokenParameterCount = definition.signature().tokenParameters().size();
		var tokenParameterDescs = tokenParameterDescs(definition.signature());
		var parameterDescs = definition.signature().parameters().stream()
			.map(parameter -> tokenAsClassDesc(parameter.paramType()))
			.toList();
		var constructorDescs = new ArrayList<ClassDesc>(tokenParameterDescs);
		constructorDescs.addAll(parameterDescs);

		var bytes = CLASS_FILE.build(classDesc, classBuilder -> {
			classBuilder
				.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL | ClassFile.ACC_SUPER)
				.withSuperclass(superclassDesc)
				.withInterfaceSymbols(traitDesc);

			for(int i = 0; i < tokenParameterCount; ++i) {
				classBuilder.withField(recordTokenParameterFieldName(i), tokenParameterDescs.get(i), ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			for(int i = 0; i < parameterDescs.size(); ++i) {
				classBuilder.withField(instanceParameterFieldName(i), parameterDescs.get(i), ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL);
			}

			classBuilder.withMethodBody(
				ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs),
				ClassFile.ACC_PUBLIC,
				codeBuilder -> {
					codeBuilder
						.aload(0)
						.invokespecial(superclassDesc, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void);

					var localSlot = 1;
					for(int i = 0; i < tokenParameterCount; ++i) {
						var tokenParameterDesc = tokenParameterDescs.get(i);
						codeBuilder
							.aload(0)
							.loadLocal(TypeKind.from(tokenParameterDesc).asLoadable(), localSlot)
							.putfield(classDesc, recordTokenParameterFieldName(i), tokenParameterDesc);
						localSlot += TypeKind.from(tokenParameterDesc).asLoadable().slotSize();
					}

					for(int i = 0; i < parameterDescs.size(); ++i) {
						var parameterDesc = parameterDescs.get(i);
						var parameterKind = TypeKind.from(parameterDesc).asLoadable();
						codeBuilder
							.aload(0)
							.loadLocal(parameterKind, localSlot)
							.putfield(classDesc, instanceParameterFieldName(i), parameterDesc);
						localSlot += parameterKind.slotSize();
					}

					codeBuilder.return_();
				}
			);

			for(var method : definition.methods()) {
				if(method._abstract()) {
					throw new RuntimeException("Instance method cannot be abstract");
				}

				var implementation = method.implementation()
					.orElseThrow(() -> new RuntimeException("Instance method implementation is missing"));

				classBuilder.withMethodBody(
					ClassNaming.methodName(method.name(), method.erasedSignature()),
					functionSignatureMethodType(method.signature()),
					ClassFile.ACC_PUBLIC,
					codeBuilder -> emitFunctionImplementationBody(
						codeBuilder,
						method.signature(),
						implementation,
						Optional.of(classDesc),
						index -> codeBuilder
							.aload(codeBuilder.receiverSlot())
							.getfield(
								classDesc,
								recordTokenParameterFieldName(index),
								tokenParameterDescs.get(index)
							)
					)
				);
			}
		});

		writeEntry(classEntryName(classDesc), bytes);
	}

	private String instanceParameterFieldName(int index) {
		return ":pv" + index;
	}

	private boolean isCoreExceptionTrait(Token.Trait trait) {
		var traitInfo = program.getTraitInfo(trait.traitId());
		return switch(traitInfo.importSpecifier()) {
			case ImportSpecifier.Global global -> {
				if(!(global.name() instanceof Identifier.Named(var name)) || !name.equals("Exception")) {
					yield false;
				}

				var moduleInfo = program.getModuleInfo(global.moduleId());
				var tubeInfo = program.getTubeInfo(moduleInfo.tubeId());
				yield tubeInfo.tubeName().equals(new TubeName("Argon", List.of("Core"))) &&
					moduleInfo.path().equals(new ModulePath(List.of("Exception")));
			}
			case ImportSpecifier.Local _ -> false;
		};
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
		private static final ClassDesc CD_CLASS = ClassDesc.of("java.lang.Class");
		private static final ClassDesc CD_UNSUPPORTED_OPERATION_EXCEPTION =
			ClassDesc.of("java.lang.UnsupportedOperationException");

		public BlockEmitter(
			CodeBuilder cb,
			int[] tokenSlots,
			ClassDesc[] tokenTypes,
			TypeKind[] tokenKinds,
			ParentTokenParameterLoader parentTokenParameterLoader,
			int[] registerSlots,
			Token[] registerTokens,
			ClassDesc[] registerTypes,
			TypeKind[] registerKinds,
			TypeKind returnKind
		) {
			this.cb = cb;
			this.tokenSlots = tokenSlots;
			this.tokenTypes = tokenTypes;
			this.tokenKinds = tokenKinds;
			this.parentTokenParameterLoader = parentTokenParameterLoader;
			this.registerSlots = registerSlots;
			this.registerTokens = registerTokens;
			this.registerTypes = registerTypes;
			this.registerKinds = registerKinds;
			this.returnKind = returnKind;
		}

		private final CodeBuilder cb;
		private final int[] tokenSlots;
		private final ClassDesc[] tokenTypes;
		private final TypeKind[] tokenKinds;
		private final ParentTokenParameterLoader parentTokenParameterLoader;
		private final int[] registerSlots;
		private final Token[] registerTokens;
		private final ClassDesc[] registerTypes;
		private final TypeKind[] registerKinds;
		private final TypeKind returnKind;

		private final Map<BlockId, BlockLabels> blocks = new HashMap<>();
		private Integer returnValueSlot = null;
		private ReturnMode returnMode = new ReturnMode.Direct();

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
					switch(returnMode) {
						case ReturnMode.Direct() -> {
							loadRegister(ret.src());
							emitReturnFromStack();
						}

						case ReturnMode.Branch(var label) -> {
							loadRegister(ret.src());
							cb.storeLocal(returnKind, getReturnValueSlot());
							cb.goto_(label);
						}
					}
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

					blocks.remove(blockInsn.blockId());
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

				case Instruction.EnumVariantLiteral enumVariantLiteral -> {
					var enumDesc = tokenAsClassDesc(enumVariantLiteral.enumType());
					var variantName = program.getEnumVariantInfo(enumVariantLiteral.variantId()).name();
					var variantDesc = enumVariantClassDesc(enumDesc, variantName);
					var enumToken = switch(enumVariantLiteral.enumType()) {
						case Token.Enum e -> e;
						default -> throw new IllegalArgumentException("Enum variant literal type is not an enum");
					};
					var enumTokenArgs = enumToken.args();
					var variantDefinition = enumVariantDefinition(enumVariantLiteral.variantId());
					var constructorDescs = new ArrayList<ClassDesc>(tokenParameterDescs(enumDefinition(enumToken.enumId()).signature()));
					constructorDescs.addAll(tokenParameterDescs(variantDefinition.signature()));
					for(var arg : enumVariantLiteral.args()) {
						constructorDescs.add(registerType(arg));
					}
					for(var field : enumVariantLiteral.fields()) {
						constructorDescs.add(registerType(field.value()));
					}

					cb
						.new_(variantDesc)
						.dup();
					for(var tokenArg : enumTokenArgs) {
						emitTokenValue(tokenArg);
					}
					for(var tokenArg : enumVariantLiteral.tokenArgs()) {
						emitTokenValue(tokenArg);
					}
					for(var arg : enumVariantLiteral.args()) {
						loadRegister(arg);
					}
					for(var field : enumVariantLiteral.fields()) {
						loadRegister(field.value());
					}
					cb.invokespecial(
						variantDesc,
						ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs.toArray(ClassDesc[]::new))
					);
					storeRegister(enumVariantLiteral.dest());
				}

				case Instruction.Finally finallyInsn -> emitFinally(finallyInsn);

				case Instruction.FunctionCall call -> {
					var function = program.getFunctionInfo(call.functionId());
					List<Token> tokenArgs = call.tokenArgs();
					List<RegisterId> args = call.args();
					var owner = function.definingClass();
					var name = function.name();
					var methodType = function.descriptor();

					for(var tokenArg : tokenArgs) {
						emitTokenValue(tokenArg);
					}
					for(var arg : args) {
						loadRegister(arg);
					}

					emitFunctionResult(
						call.dest(),
						() -> cb.invokestatic(owner, name, methodType),
						() -> cb.invokedynamic(DynamicCallSiteDesc.of(
							BSM_LAMBDA_METAFACTORY,
							"step",
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, methodType.parameterArray()),
							MethodTypeDesc.of(CD_TRAMPOLINE),
							MethodHandleDesc.ofMethod(
								DirectMethodHandleDesc.Kind.STATIC,
								owner,
								name,
								methodType
							),
							MethodTypeDesc.of(CD_TRAMPOLINE)
						))
					);
				}

				case Instruction.FunctionObjectCall call -> {
					loadRegister(call.function());
					loadRegister(call.arg());
					boxValue(registerKind(call.arg()));
					emitFunctionResult(
						call.dest(),
						() -> cb.invokeinterface(
							CD_FUNCTION,
							"apply",
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object)
						),
						() -> emitFunctionObjectCallThunk(
							CD_FUNCTION,
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object),
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, CD_FUNCTION, ConstantDescs.CD_Object)
						)
					);
				}

				case Instruction.FunctionObjectErasedCall call -> {
					loadRegister(call.function());
					emitFunctionResult(
						call.dest(),
						() -> cb.invokeinterface(
							CD_FUNCTION_ERASED,
							"apply",
							MethodTypeDesc.of(CD_TRAMPOLINE)
						),
						() -> emitFunctionObjectCallThunk(
							CD_FUNCTION_ERASED,
							MethodTypeDesc.of(CD_TRAMPOLINE),
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, CD_FUNCTION_ERASED)
						)
					);
				}

				case Instruction.FunctionObjectTokenCall call -> {
					loadRegister(call.function());
					emitTokenValue(call.arg());
					emitFunctionResult(
						call.dest(),
						() -> cb.invokeinterface(
							CD_FUNCTION_TOKEN,
							"apply",
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object)
						),
						() -> emitFunctionObjectCallThunk(
							CD_FUNCTION_TOKEN,
							MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object),
							MethodTypeDesc.of(CD_TRAMPOLINE_THUNK, CD_FUNCTION_TOKEN, ConstantDescs.CD_Object)
						)
					);
				}

				case Instruction.IfElse ifElse -> {
					var whenTrue = cb.newLabel();
					var whenFalse = cb.newLabel();
					var end = cb.newLabel();

					blocks.put(ifElse.whenTrueBlockId(), new BlockLabels(whenTrue, whenTrue));
					blocks.put(ifElse.whenFalseBlockId(), new BlockLabels(whenFalse, whenFalse));
					emitBlock(ifElse.condition());
					blocks.remove(ifElse.whenTrueBlockId());
					blocks.remove(ifElse.whenFalseBlockId());

					cb.labelBinding(whenTrue);
					emitBlock(ifElse.whenTrue());
					cb.goto_(end);

					cb.labelBinding(whenFalse);
					emitBlock(ifElse.whenFalse());

					cb.labelBinding(end);
				}

				case Instruction.InstanceMethodCall call -> {
					var methodInfo = program.getMethodInfo(call.methodId());
					var owner = tokenAsClassDesc(call.instanceType());
					var name = ClassNaming.methodName(methodInfo.name(), methodInfo.erasedSignature());
					var methodType = functionSignatureMethodType(methodInfo.signature());
					var ownerIsInterface = call.instanceType() instanceof Token.Trait;

					loadRegister(call.instanceObject());
					for(var tokenArg : call.tokenArgs()) {
						emitTokenValue(tokenArg);
					}
					for(var arg : call.args()) {
						loadRegister(arg);
					}

					emitFunctionResult(
						call.dest(),
						() -> {
							if(ownerIsInterface) {
								cb.invokeinterface(owner, name, methodType);
							}
							else {
								cb.invokevirtual(owner, name, methodType);
							}
						},
						() -> cb.invokedynamic(DynamicCallSiteDesc.of(
							BSM_LAMBDA_METAFACTORY,
							"step",
							instanceMethodThunkFactoryType(owner, methodType),
							MethodTypeDesc.of(CD_TRAMPOLINE),
							MethodHandleDesc.ofMethod(
								ownerIsInterface
									? DirectMethodHandleDesc.Kind.INTERFACE_VIRTUAL
									: DirectMethodHandleDesc.Kind.VIRTUAL,
								owner,
								name,
								methodType
							),
							MethodTypeDesc.of(CD_TRAMPOLINE)
						))
					);
				}
				
				case Instruction.IsEnumVariantOrBreak isEnumVariantOrBreak -> {
					var variantDesc = enumVariantClassDesc(
						tokenAsClassDesc(isEnumVariantOrBreak.enumType()),
						program.getEnumVariantInfo(isEnumVariantOrBreak.variantId()).name()
					);
					var variantDefinition = enumVariantDefinition(isEnumVariantOrBreak.variantId());
					var argOffset = variantDefinition.signature().tokenParameters().size();

					loadRegister(isEnumVariantOrBreak.value());
					cb
						.instanceOf(variantDesc)
						.ifeq(blocks.get(isEnumVariantOrBreak.notVariantBlockId()).end);

					for(int i = 0; i < isEnumVariantOrBreak.args().size(); ++i) {
						var arg = isEnumVariantOrBreak.args().get(i);
						loadRegister(isEnumVariantOrBreak.value());
						cb
							.checkcast(variantDesc)
							.getfield(variantDesc, ":pv" + (argOffset + i), registerType(arg));
						storeRegister(arg);
					}

					for(var fieldExtractor : isEnumVariantOrBreak.fieldExtractors()) {
						var fieldInfo = program.getRecordFieldInfo(fieldExtractor.fieldId());
						loadRegister(isEnumVariantOrBreak.value());
						cb
							.checkcast(variantDesc)
							.getfield(variantDesc, ClassNaming.fieldName(fieldInfo.name()), registerType(fieldExtractor.r()));
						storeRegister(fieldExtractor.r());
					}
				}
				case Instruction.LoadInstanceField loadInstanceField -> {
					var instanceDesc = tokenAsClassDesc(loadInstanceField.instanceType());
					loadRegister(loadInstanceField.instanceObject());
					cb.getfield(
						instanceDesc,
						instanceParameterFieldName(loadInstanceField.parameterIndex().toBigInteger().intValueExact()),
						registerType(loadInstanceField.dest())
					);
					storeRegister(loadInstanceField.dest());
				}

				case Instruction.LoadReference loadReference -> {
					loadRegister(loadReference.ref());
					cb.invokevirtual(
						CD_REF_CELL,
						"get",
						MethodTypeDesc.of(ConstantDescs.CD_Object)
					);
					unboxValue(registerType(loadReference.dest()));
					storeRegister(loadReference.dest());
				}

				case Instruction.LoadToken loadToken -> {
					emitTokenValue(loadToken.token());
					storeRegister(loadToken.dest());
				}

				case Instruction.NewInstance newInstance -> {
					var instanceInfo = program.getInstanceInfo(newInstance.instanceId());
					var instanceDesc = ClassNaming.typeDefinitionClassDescriptor(program, instanceInfo.importSpecifier());
					var constructorDescs = new ArrayList<ClassDesc>(tokenParameterDescs(instanceInfo.signature()));
					for(var arg : newInstance.args()) {
						constructorDescs.add(registerType(arg));
					}

					cb
						.new_(instanceDesc)
						.dup();
					for(var tokenArg : newInstance.tokenArgs()) {
						emitTokenValue(tokenArg);
					}
					for(var arg : newInstance.args()) {
						loadRegister(arg);
					}
					cb.invokespecial(
						instanceDesc,
						ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs.toArray(ClassDesc[]::new))
					);
					storeRegister(newInstance.dest());
				}

				case Instruction.NewReference newReference -> {
					cb
						.new_(CD_REF_CELL)
						.dup();
					loadRegister(newReference.value());
					boxValue(registerKind(newReference.value()));
					cb.invokespecial(
						CD_REF_CELL,
						ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object)
					);
					storeRegister(newReference.dest());
				}
				case Instruction.PartiallyAppliedFunction partial ->
					emitPartiallyAppliedFunction(
						partial.functionId(),
						partial.dest(),
						partial.tokenArgs(),
						partial.args(),
						PartialApplicationKind.FUNCTION
					);
				case Instruction.PartiallyAppliedFunctionErased partial ->
					emitPartiallyAppliedFunction(
						partial.functionId(),
						partial.dest(),
						partial.tokenArgs(),
						partial.args(),
						PartialApplicationKind.ERASED
					);
				case Instruction.PartiallyAppliedTokenFunction partial ->
					emitPartiallyAppliedFunction(
						partial.functionId(),
						partial.dest(),
						partial.tokenArgs(),
						partial.args(),
						PartialApplicationKind.TOKEN
					);
				case Instruction.Raise raise -> {
					loadRegister(raise.exception());
					cb
						.checkcast(CD_ARGON_EXCEPTION)
						.athrow();
				}
				case Instruction.RecordFieldLoad recordFieldLoad -> {
					var fieldInfo = program.getRecordFieldInfo(recordFieldLoad.fieldId());
					var owner = recordFieldOwner(fieldInfo);
					var fieldDesc = registerType(recordFieldLoad.dest());
					loadRegister(recordFieldLoad.recordValue());
					cb.getfield(owner, ClassNaming.fieldName(fieldInfo.name()), fieldDesc);
					storeRegister(recordFieldLoad.dest());
				}

				case Instruction.RecordFieldStore recordFieldStore -> {
					var fieldInfo = program.getRecordFieldInfo(recordFieldStore.fieldId());
					var owner = recordFieldOwner(fieldInfo);
					var fieldDesc = registerType(recordFieldStore.fieldValue());
					loadRegister(recordFieldStore.recordValue());
					loadRegister(recordFieldStore.fieldValue());
					cb.putfield(owner, ClassNaming.fieldName(fieldInfo.name()), fieldDesc);
				}

				case Instruction.RecordLiteral recordLiteral -> {
					var recordDesc = tokenAsClassDesc(recordLiteral.recordType());
					var record = switch(recordLiteral.recordType()) {
						case Token.Record r -> r;
						default -> throw new IllegalArgumentException("Record literal type is not a record");
					};
					var recordTokenArgs = record.args();
					var fieldDescs = recordLiteral.fields().stream()
						.map(field -> registerType(field.value()))
						.toArray(ClassDesc[]::new);
					var constructorDescs = new ArrayList<ClassDesc>(tokenParameterDescs(program.getRecordInfo(record.recordId()).signature()));
					constructorDescs.addAll(List.of(fieldDescs));
					cb
						.new_(recordDesc)
						.dup();
					for(var tokenArg : recordTokenArgs) {
						emitTokenValue(tokenArg);
					}
					for(var field : recordLiteral.fields()) {
						loadRegister(field.value());
					}
					cb.invokespecial(
						recordDesc,
						ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs.toArray(ClassDesc[]::new))
					);
					storeRegister(recordLiteral.dest());
				}
				case Instruction.Tuple tuple -> {
					emitTupleValue(tuple.values());
					storeRegister(tuple.dest());
				}

				case Instruction.TupleElement tupleElement -> {
					loadRegister(tupleElement.src());
					emitTupleElement(tupleElement.src(), tupleElement.elementIndex().toBigInteger().intValueExact());
					unboxValue(registerType(tupleElement.dest()));
					storeRegister(tupleElement.dest());
				}

				case Instruction.Unbox unbox -> {
					loadRegister(unbox.value());
					unboxValue(tokenAsClassDesc(unbox.type()));
					storeRegister(unbox.dest());
				}
				
				case Instruction.UpdateReference updateReference -> {
					loadRegister(updateReference.ref());
					loadRegister(updateReference.value());
					boxValue(registerKind(updateReference.value()));
					cb.invokevirtual(
						CD_REF_CELL,
						"set",
						MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object)
					);
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
			cb
				.new_(tupleDesc)
				.dup();

			if(values.size() <= 10) {
				for(var value : values) {
					loadRegister(value);
					boxValue(registerKind(value));
				}
				cb.invokespecial(
					tupleDesc,
					ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(
						ConstantDescs.CD_void,
						Collections.nCopies(values.size(), ConstantDescs.CD_Object).toArray(ClassDesc[]::new)
					)
				);
			}
			else {
				for(int i = 0; i < 10; ++i) {
					loadRegister(values.get(i));
					boxValue(registerKind(values.get(i)));
				}
				emitTupleValue(values.subList(10, values.size()));
				var parameterTypes = new ArrayList<ClassDesc>(Collections.nCopies(10, ConstantDescs.CD_Object));
				parameterTypes.add(CD_TUPLE_BASE);
				cb.invokespecial(
					tupleDesc,
					ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, parameterTypes.toArray(ClassDesc[]::new))
				);
			}
		}

		private void emitTupleElement(RegisterId src, int index) {
			var tupleToken = registerToken(src);
			if(!(tupleToken instanceof Token.Tuple tuple)) {
				throw new IllegalArgumentException("TupleElement source register is not a tuple");
			}

			int remainingIndex = index;
			int remainingSize = tuple.elements().size();
			while(remainingIndex >= 10) {
				cb
					.checkcast(CD_TUPLE_XL)
					.invokevirtual(
						CD_TUPLE_XL,
						"rest",
						MethodTypeDesc.of(CD_TUPLE_BASE)
					);
				remainingIndex -= 10;
				remainingSize -= 10;
			}

			var tupleDesc = tupleClassDesc(remainingSize);
			cb
				.checkcast(tupleDesc)
				.invokevirtual(
					tupleDesc,
					"element" + remainingIndex,
					MethodTypeDesc.of(ConstantDescs.CD_Object)
				);
		}

		private ClassDesc tupleClassDesc(int size) {
			if(size > 10) {
				return CD_TUPLE_XL;
			}

			return ClassDesc.of("dev.argon.runtime", "Tuple" + size);
		}

		private ClassDesc recordFieldOwner(ProgramModel.RecordFieldInfo fieldInfo) {
			if(fieldInfo.ownerType() != ProgramModel.RecordFieldInfo.OwnerType.RECORD) {
				throw new UnsupportedOperationException("Enum variant record fields are not implemented");
			}

			return ClassNaming.typeDefinitionClassDescriptor(
				program,
				program.getRecordInfo(fieldInfo.recordId()).importSpecifier()
			);
		}

		private MethodTypeDesc functionCallMethodType(int tokenArgCount, List<RegisterId> args) {
			var parameterTypes = new ArrayList<ClassDesc>();
			for(int i = 0; i < tokenArgCount; ++i) {
				parameterTypes.add(ClassDesc.of("dev.argon.runtime.Token"));
			}
			for(var arg : args) {
				parameterTypes.add(registerType(arg));
			}

			return MethodTypeDesc.of(CD_TRAMPOLINE, parameterTypes);
		}

		private void emitPartiallyAppliedFunction(
			UnsignedBigInteger functionId,
			RegisterId dest,
			List<Token> tokenArgs,
			List<RegisterId> args,
			PartialApplicationKind kind
		) {
			var function = program.getFunctionInfo(functionId);
			var owner = function.definingClass();
			var name = function.name();
			var interfaceDesc = partialApplicationInterface(kind);
			var nextArgType = partialApplicationNextArgType(kind, dest);
			var implementationMethodType = function.descriptor();
			var factoryType = partialApplicationFactoryType(
				interfaceDesc,
				function.signature(),
				tokenArgs,
				args
			);
			var targetHandle = MethodHandleDesc.ofMethod(
				DirectMethodHandleDesc.Kind.STATIC,
				owner,
				name,
				implementationMethodType
			);

			for(var tokenArg : tokenArgs) {
				emitTokenValue(tokenArg);
			}
			for(var arg : args) {
				loadRegister(arg);
			}

			var callSite = switch(kind) {
				case FUNCTION -> DynamicCallSiteDesc.of(
					BSM_LAMBDA_METAFACTORY,
					"apply",
					factoryType,
					MethodTypeDesc.of(CD_TRAMPOLINE, ConstantDescs.CD_Object),
					targetHandle,
					MethodTypeDesc.of(CD_TRAMPOLINE, nextArgType.orElseThrow())
				);
				case TOKEN -> DynamicCallSiteDesc.of(
					BSM_PARTIAL_TOKEN_FUNCTION,
					"apply",
					factoryType,
					targetHandle,
					Integer.valueOf(tokenArgs.size())
				);
				case ERASED -> DynamicCallSiteDesc.of(
					BSM_LAMBDA_METAFACTORY,
					"apply",
					factoryType,
					MethodTypeDesc.of(CD_TRAMPOLINE),
					targetHandle,
					MethodTypeDesc.of(CD_TRAMPOLINE)
				);
			};
			cb.invokedynamic(callSite);
			storeRegister(dest);
		}

		private ClassDesc partialApplicationInterface(PartialApplicationKind kind) {
			return switch(kind) {
				case FUNCTION -> CD_FUNCTION;
				case TOKEN -> CD_FUNCTION_TOKEN;
				case ERASED -> CD_FUNCTION_ERASED;
			};
		}

		private Optional<ClassDesc> partialApplicationNextArgType(PartialApplicationKind kind, RegisterId dest) {
			return switch(kind) {
				case FUNCTION -> {
					if(!(registerToken(dest) instanceof Token.Function function)) {
						throw new IllegalArgumentException("PartiallyAppliedFunction destination is not a function");
					}
					yield Optional.of(tokenAsClassDesc(function.input()));
				}
				case TOKEN, ERASED -> Optional.empty();
			};
		}

		private MethodTypeDesc partialApplicationFactoryType(
			ClassDesc interfaceDesc,
			FunctionSignature signature,
			List<Token> tokenArgs,
			List<RegisterId> args
		) {
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

		private void emitFunctionResult(
			FunctionResult dest,
			Runnable emitCall,
			Runnable emitThunk
		) {
			switch(dest) {
				case FunctionResult.Discard ignored -> {
					emitCall.run();
					emitResolveTrampoline();
					cb.pop();
				}

				case FunctionResult.Register(var id) -> {
					emitCall.run();
					emitResolveTrampoline();
					unboxValue(registerType(id));
					storeRegister(id);
				}

				case FunctionResult.ReturnValue ignored -> {
					emitThunk.run();
					cb.areturn();
				}
			}
		}

		private void emitResolveTrampoline() {
			cb.invokestatic(
				CD_TRAMPOLINE,
				"resolve",
				MethodTypeDesc.of(ConstantDescs.CD_Object, CD_TRAMPOLINE)
			);
		}

		private void emitFunctionObjectCallThunk(
			ClassDesc owner,
			MethodTypeDesc applyMethodType,
			MethodTypeDesc thunkFactoryType
		) {
			cb.invokedynamic(DynamicCallSiteDesc.of(
				BSM_LAMBDA_METAFACTORY,
				"step",
				thunkFactoryType,
				MethodTypeDesc.of(CD_TRAMPOLINE),
				MethodHandleDesc.ofMethod(
					DirectMethodHandleDesc.Kind.INTERFACE_VIRTUAL,
					owner,
					"apply",
					applyMethodType
				),
				MethodTypeDesc.of(CD_TRAMPOLINE)
			));
		}

		private void emitTokenValue(Token token) {
			switch(token) {
				case Token.TokenParameter tokenParameter -> {
					var index = tokenParameter.index().toBigInteger().intValueExact();
					cb.loadLocal(tokenKinds[index], tokenSlots[index]);
				}
				case Token.ParentTokenParameter parentTokenParameter ->
					parentTokenParameterLoader.load(parentTokenParameter.index().toBigInteger().intValueExact());

				case Token.InstanceValue instanceValue -> {
					var instanceInfo = program.getInstanceInfo(instanceValue.instanceId());
					var instanceDesc = ClassNaming.typeDefinitionClassDescriptor(program, instanceInfo.importSpecifier());
					var constructorDescs = tokenParameterDescs(instanceInfo.signature());

					cb
						.new_(instanceDesc)
						.dup();
					for(var tokenArg : instanceValue.args()) {
						emitTokenValue(tokenArg);
					}
					cb.invokespecial(
						instanceDesc,
						ConstantDescs.INIT_NAME,
						MethodTypeDesc.of(ConstantDescs.CD_void, constructorDescs.toArray(ClassDesc[]::new))
					);
				}

				case Token.Builtin(var builtin) -> {
					switch(builtin) {
						case BuiltinType.Array(var elementType) -> emitTypeInfoTokenValue(token, List.of(elementType));
						case BuiltinType.Bool(), BuiltinType.Int(), BuiltinType.Never(), BuiltinType.String() ->
							emitTypeInfoTokenValue(token, List.of());
						case BuiltinType.Conjunction _, BuiltinType.Disjunction _ -> throw new UnsupportedOperationException(
							"TypeInfo is not implemented for JVM backend token: " + builtin.getClass().getSimpleName()
						);
					}
				}
				case Token.Boxed _, Token.TypeInfo() -> emitTypeInfoTokenValue(token, List.of());
				case Token.Enum enumToken -> emitTypeInfoTokenValue(token, enumToken.args());
				case Token.Function function -> emitTypeInfoTokenValue(token, List.of(function.input(), function.output()));
				case Token.FunctionErased function -> emitTypeInfoTokenValue(token, List.of(function.output()));
				case Token.FunctionToken function -> emitTypeInfoTokenValue(token, List.of(function.tokenKind(), function.output()));
				case Token.InstanceType instanceType -> emitTypeInfoTokenValue(token, instanceType.args());
				case Token.Record record -> emitTypeInfoTokenValue(token, record.args());
				case Token.RefCell refCell -> emitTypeInfoTokenValue(token, List.of(refCell.inner()));
				case Token.Trait trait -> emitTypeInfoTokenValue(token, trait.args());
				case Token.Tuple tuple -> emitTypeInfoTokenValue(token, tuple.elements());
			}
		}

		private void emitTypeInfoTokenValue(
			Token token,
			List<Token> args
		) {
			cb
				.new_(CD_TYPE_INFO)
				.dup()
				.ldc(tokenAsClassDesc(token));

			if(args.isEmpty()) {
				cb.invokespecial(
					CD_TYPE_INFO,
					ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, CD_CLASS)
				);
			}
			else {
				cb.loadConstant(args.size());
				cb.anewarray(CD_TOKEN);
				for(int i = 0; i < args.size(); ++i) {
					cb
						.dup()
						.loadConstant(i);
					emitTokenValue(args.get(i));
					cb.aastore();
				}
				cb.invokespecial(
					CD_TYPE_INFO,
					ConstantDescs.INIT_NAME,
					MethodTypeDesc.of(ConstantDescs.CD_void, CD_CLASS, CD_TOKEN.arrayType())
				);
			}
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
					if(stringConcat.args().isEmpty()) {
						cb.ldc("");
					}
					else if(stringConcat.args().size() == 1) {
						loadRegister(stringConcat.args().get(0));
					}
					else if(stringConcat.args().size() == 2) {
						loadRegister(stringConcat.args().get(0));
						loadRegister(stringConcat.args().get(1));
						cb.invokevirtual(
							ConstantDescs.CD_String,
							"concat",
							MethodTypeDesc.of(ConstantDescs.CD_String, ConstantDescs.CD_String)
						);
					}
					else {
						for(var arg : stringConcat.args()) {
							loadRegister(arg);
						}

						var argTypes = Collections.nCopies(
							stringConcat.args().size(),
							ConstantDescs.CD_String
						).toArray(ClassDesc[]::new);
						cb.invokedynamic(DynamicCallSiteDesc.of(
							BSM_STRING_CONCAT_WITH_CONSTANTS,
							"makeConcatWithConstants",
							MethodTypeDesc.of(ConstantDescs.CD_String, argTypes),
							"\u0001".repeat(stringConcat.args().size())
						));
					}
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

		private void emitFinally(Instruction.Finally finallyInsn) {
			var start = cb.newLabel();
			var end = cb.newLabel();
			var handler = cb.newLabel();
			var done = cb.newLabel();
			var exceptionSlot = cb.allocateLocal(TypeKind.REFERENCE);
			var originalBlocks = new HashMap<>(blocks);
			var finallyJumpLabels = new LinkedHashMap<FinallyJumpTarget, Label>();
			for(var target : scanFinallyJumpTargets(finallyInsn.action(), originalBlocks)) {
				finallyJumpLabels.put(target, cb.newLabel());
			}

			var oldReturnMode = returnMode;
			Label returnFinallyLabel = null;
			if(containsReturn(finallyInsn.action())) {
				returnFinallyLabel = cb.newLabel();
				returnMode = new ReturnMode.Branch(returnFinallyLabel);
			}

			cb.labelBinding(start);
			try {
				installFinallyJumpLabels(originalBlocks, finallyJumpLabels);
				emitBlock(finallyInsn.action());
			}
			finally {
				blocks.clear();
				blocks.putAll(originalBlocks);
				returnMode = oldReturnMode;
			}
			cb.labelBinding(end);

			emitBlock(finallyInsn.ensuring());
			cb.goto_(done);

			for(var entry : finallyJumpLabels.entrySet()) {
				cb.labelBinding(entry.getValue());
				emitBlock(finallyInsn.ensuring());
				cb.goto_(entry.getKey().label(originalBlocks));
			}

			if(returnFinallyLabel != null) {
				cb.labelBinding(returnFinallyLabel);
				emitBlock(finallyInsn.ensuring());
				cb.loadLocal(returnKind, getReturnValueSlot());
				emitReturnFromStack();
			}

			cb.labelBinding(handler);
			cb.storeLocal(TypeKind.REFERENCE, exceptionSlot);
			emitBlock(finallyInsn.ensuring());
			cb.loadLocal(TypeKind.REFERENCE, exceptionSlot);
			cb.athrow();

			cb.labelBinding(done);
			cb.exceptionCatchAll(start, end, handler);
		}

		private Set<FinallyJumpTarget> scanFinallyJumpTargets(
			Block block,
			Map<BlockId, BlockLabels> knownBlocks
		) {
			var targets = new LinkedHashSet<FinallyJumpTarget>();
			new InstructionScanner() {
				@Override
				protected void visitInstruction(Instruction instruction) {
					switch(instruction) {
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
			}.scan(block);

			return targets;
		}

		private boolean containsReturn(Block block) {
			var scanner = new InstructionScanner() {
				boolean containsReturn = false;

				@Override
				protected void visitInstruction(Instruction instruction) {
					if(instruction instanceof Instruction.Return) {
						containsReturn = true;
					}
				}
			};
			scanner.scan(block);

			return scanner.containsReturn;
		}

		private void installFinallyJumpLabels(
			Map<BlockId, BlockLabels> originalBlocks,
			Map<FinallyJumpTarget, Label> finallyJumpLabels
		) {
			for(var entry : originalBlocks.entrySet()) {
				var blockId = entry.getKey();
				var labels = entry.getValue();
				var start = finallyJumpLabels.getOrDefault(
					new FinallyJumpTarget(blockId, FinallyJumpTargetKind.START),
					labels.start
				);
				var end = finallyJumpLabels.getOrDefault(
					new FinallyJumpTarget(blockId, FinallyJumpTargetKind.END),
					labels.end
				);

				blocks.put(blockId, new BlockLabels(start, end));
			}
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

		private void unboxValue(ClassDesc type) {
			switch(TypeKind.from(type)) {
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

				case REFERENCE -> cb.checkcast(type);

				case VOID -> throw new IllegalArgumentException("Cannot unbox void");
			}
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

		private int getReturnValueSlot() {
			if(returnValueSlot == null) {
				returnValueSlot = cb.allocateLocal(returnKind);
			}

			return returnValueSlot;
		}

		private void emitReturnFromStack() {
			boxValue(returnKind);
			cb
				.invokestatic(
					CD_TRAMPOLINE_RESULT,
					"of",
					MethodTypeDesc.of(CD_TRAMPOLINE_RESULT, ConstantDescs.CD_Object)
				)
				.areturn();
		}

		private interface ParentTokenParameterLoader {
			void load(int index);
		}

		private enum PartialApplicationKind {
			FUNCTION,
			TOKEN,
			ERASED,
		}

		private record BlockLabels(
			Label start,
			Label end
		) {
		}

		private record FinallyJumpTarget(
			BlockId blockId,
			FinallyJumpTargetKind kind
		) {
			private Label label(Map<BlockId, BlockLabels> blocks) {
				var labels = blocks.get(blockId);
				return switch(kind) {
					case START -> labels.start;
					case END -> labels.end;
				};
			}
		}

		private enum FinallyJumpTargetKind {
			START,
			END
		}

		private sealed interface ReturnMode {
			record Direct() implements ReturnMode {}
			record Branch(Label branch) implements ReturnMode {}
		}
	}
}
