package dev.argon.backend.ir;

import com.google.common.collect.ImmutableMap;
import dev.argon.backend.codegen.ProgramModel;
import dev.argon.backend.io.InputFile;
import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.ESExpr;
import dev.argon.esexpr.ESExprBinaryReader;
import dev.argon.esexpr.SyntaxException;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.*;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.lang.constant.*;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public final class IrReader {
	private static final JvmPlatformTubeMetadata EMPTY_PLATFORM_METADATA =
		new JvmPlatformTubeMetadata(
			Optional.empty(),
			Optional.empty(),
			Optional.empty()
		);

	private @Nullable TubeHeader header;
	private @Nullable TubeMetadata metadata;
	private final Map<UnsignedBigInteger, ModuleModelBuilder> modules = new HashMap<>();

	private final Map<UnsignedBigInteger, TubeFileEntry.ModuleReference> moduleRefMap = new HashMap<>();
	private final Map<UnsignedBigInteger, FunctionEntry> functionMap = new HashMap<>();
	private final Map<UnsignedBigInteger, RecordEntry> recordMap = new HashMap<>();
	private final Map<UnsignedBigInteger, RecordFieldEntry> recordFieldMap = new HashMap<>();
	private final Map<UnsignedBigInteger, EnumEntry> enumMap = new HashMap<>();
	private final Map<UnsignedBigInteger, TubeFileEntry.EnumVariantReference> enumVariantMap = new HashMap<>();
	private final Map<UnsignedBigInteger, TraitEntry> traitMap = new HashMap<>();
	private final Map<UnsignedBigInteger, MethodEntry> methodMap = new HashMap<>();
	private final Map<UnsignedBigInteger, InstanceEntry> instanceMap = new HashMap<>();

	public static ProgramModel readFile(InputFile file) throws IOException, SyntaxException, DecodeException {
		try(var input = file.open()) {
			return read(input);
		}
	}

	public static ProgramModel read(InputStream input) throws IOException, SyntaxException, DecodeException {
		var reader = new IrReader();
		var binaryReader = new ESExprBinaryReader(input);

		ESExpr expr;
		while((expr = binaryReader.tryRead()) != null) {
			reader.recordEntry(TubeFileEntry.codec().decode(expr));
		}

		return reader.build();
	}

	public static ProgramModel read(Iterable<TubeFileEntry> entries) throws DecodeException {
		var reader = new IrReader();
		for(var entry : entries) {
			reader.recordEntry(entry);
		}

		return reader.build();
	}

	public ProgramModel build() throws DecodeException {
		if(header == null) {
			throw new IllegalStateException("VMIR header is missing");
		}

		if(metadata == null) {
			throw new IllegalStateException("VMIR metadata is missing");
		}

		var referencePlatformMetadata = ImmutableMap.<TubeName, JvmPlatformTubeMetadata>builder();
		for(var ref : metadata.referencedTubes()) {
			referencePlatformMetadata.put(
				ref.name(),
				decodePlatformMetadata(ref.metadata())
			);
		}

		var decodedMetadata = new ProgramModel.DecodedMetadata(
			decodePlatformMetadata(metadata.platformMetadata()),
			referencePlatformMetadata.build()
		);

		var moduleModels = new ArrayList<ProgramModel.ModuleModel>();
		var declaredModules = metadata.modules();
		for(int i = 0; i < declaredModules.size(); ++i) {
			var moduleBuilder = modules.get(UnsignedBigInteger.valueOf(i));
			var exports =
				moduleBuilder == null
					? List.<ProgramModel.ModuleExportEntry>of()
					: List.copyOf(moduleBuilder.exports);

			moduleModels.add(new ProgramModel.ModuleModel(declaredModules.get(i).path(), exports, UnsignedBigInteger.valueOf(i)));
		}

		return new ProgramModelImpl(
			header,
			metadata,
			decodedMetadata,
			List.copyOf(moduleModels),
			Map.copyOf(moduleRefMap),
			Map.copyOf(functionMap),
			Map.copyOf(recordMap),
			Map.copyOf(recordFieldMap),
			Map.copyOf(enumMap),
			Map.copyOf(enumVariantMap),
			Map.copyOf(traitMap),
			Map.copyOf(methodMap),
			Map.copyOf(instanceMap)
		);
	}

	public void recordEntry(TubeFileEntry entry) {
		if(header == null) {
			if(!(entry instanceof TubeFileEntry.Header headerEntry)) {
				throw new IllegalStateException("First VMIR entry must be a header");
			}

			header = headerEntry.header();
			return;
		}

		if(metadata == null) {
			if(!(entry instanceof TubeFileEntry.Metadata metadataEntry)) {
				throw new IllegalStateException("Second VMIR entry must be metadata");
			}

			metadata = metadataEntry.metadata();
			return;
		}

		ImportSpecifier importSpec;
		ProgramModel.ModuleExportEntry exportEntry;

		switch(entry) {
			case TubeFileEntry.Header ignored -> throw new IllegalStateException("Extra header not allowed in VMIR");

			case TubeFileEntry.Metadata ignored ->
				throw new IllegalStateException("Extra metadata not allowed in VMIR");

			case TubeFileEntry.ModuleReference moduleRef -> {
				moduleRefMap.put(moduleRef.moduleId(), moduleRef);
				return;
			}

			case TubeFileEntry.FunctionDefinition functionDefinition -> {
				functionMap.put(
					functionDefinition.definition().functionId(),
					new FunctionEntry.Definition(functionDefinition)
				);
				importSpec = functionDefinition.definition()._import();
				exportEntry = new ProgramModel.ModuleExportEntry.FunctionDefinition(functionDefinition);
			}

			case TubeFileEntry.FunctionReference functionRef -> {
				functionMap.put(functionRef.functionId(), new FunctionEntry.Reference(functionRef));
				return;
			}

			case TubeFileEntry.RecordDefinition recordDefinition -> {
				recordMap.put(
					recordDefinition.definition().recordId(),
					new RecordEntry.Definition(recordDefinition)
				);
				importSpec = recordDefinition.definition()._import();
				exportEntry = new ProgramModel.ModuleExportEntry.RecordDefinition(recordDefinition);
				for(var method : recordDefinition.definition().methods()) methodMap.put(method.methodId(),
					new MethodEntry.RecordOwner(new TubeFileEntry.RecordMethodReference(method.methodId(), recordDefinition.definition().recordId(), method.name(), method.erasedSignature(), method.signature())));

				for(var field : recordDefinition.definition().fields()) {
					recordFieldMap.put(
						field.fieldId(),
						new RecordFieldEntry.OfRecord(new TubeFileEntry.RecordFieldReference(
							field.fieldId(),
							recordDefinition.definition().recordId(),
							field.name(),
							field.fieldType()
						))
					);
				}
			}

			case TubeFileEntry.RecordReference recordRef -> {
				recordMap.put(recordRef.recordId(), new RecordEntry.Reference(recordRef));
				return;
			}
			case TubeFileEntry.RecordMethodReference methodRef -> { methodMap.put(methodRef.methodId(), new MethodEntry.RecordOwner(methodRef)); return; }

			case TubeFileEntry.EnumDefinition enumDefinition -> {
				enumMap.put(
					enumDefinition.definition().enumId(),
					new EnumEntry.Definition(enumDefinition)
				);
				importSpec = enumDefinition.definition()._import();
				exportEntry = new ProgramModel.ModuleExportEntry.EnumDefinition(enumDefinition);
				for(var method : enumDefinition.definition().methods()) methodMap.put(method.methodId(),
					new MethodEntry.EnumOwner(new TubeFileEntry.EnumMethodReference(method.methodId(), enumDefinition.definition().enumId(), method.name(), method.erasedSignature(), method.signature())));
				for(var variant : enumDefinition.definition().variants()) {
					enumVariantMap.put(
						variant.variantId(),
						new TubeFileEntry.EnumVariantReference(
							variant.variantId(),
							enumDefinition.definition().enumId(),
							variant.name(),
							variant.signature()
						)
					);
					for(var method : variant.methods()) methodMap.put(method.methodId(),
						new MethodEntry.EnumVariant(new TubeFileEntry.EnumVariantMethodReference(method.methodId(), variant.variantId(), method.name(), method.erasedSignature(), method.signature())));

					for(var field : variant.fields()) {
						recordFieldMap.put(
							field.fieldId(),
							new RecordFieldEntry.EnumVariant(new TubeFileEntry.EnumVariantRecordFieldReference(
								field.fieldId(),
								variant.variantId(),
								field.name(),
								field.fieldType()
							))
						);
					}
				}
			}

			case TubeFileEntry.EnumReference enumRef -> {
				enumMap.put(enumRef.enumId(), new EnumEntry.Reference(enumRef));
				return;
			}
			case TubeFileEntry.EnumMethodReference methodRef -> { methodMap.put(methodRef.methodId(), new MethodEntry.EnumOwner(methodRef)); return; }

			case TubeFileEntry.EnumVariantReference enumVariantRef -> {
				enumVariantMap.put(enumVariantRef.variantId(), enumVariantRef);
				return;
			}
			case TubeFileEntry.EnumVariantMethodReference methodRef -> { methodMap.put(methodRef.methodId(), new MethodEntry.EnumVariant(methodRef)); return; }

			case TubeFileEntry.RecordFieldReference recordFieldRef -> {
				recordFieldMap.put(
					recordFieldRef.recordFieldId(),
					new RecordFieldEntry.OfRecord(recordFieldRef)
				);
				return;
			}

			case TubeFileEntry.EnumVariantRecordFieldReference enumVariantRecordFieldRef -> {
				recordFieldMap.put(
					enumVariantRecordFieldRef.recordFieldId(),
					new RecordFieldEntry.EnumVariant(enumVariantRecordFieldRef)
				);
				return;
			}

			case TubeFileEntry.TraitDefinition traitDefinition -> {
				traitMap.put(
					traitDefinition.definition().traitId(),
					new TraitEntry.Definition(traitDefinition)
				);
				importSpec = traitDefinition.definition()._import();
				exportEntry = new ProgramModel.ModuleExportEntry.TraitDefinition(traitDefinition);
				for(var method : traitDefinition.definition().methods()) {
					methodMap.put(
						method.methodId(),
						new MethodEntry.Trait(new TubeFileEntry.TraitMethodReference(
							method.methodId(),
							traitDefinition.definition().traitId(),
							method.name(),
							method.erasedSignature(),
							method.signature()
						))
					);
				}
			}

			case TubeFileEntry.TraitReference traitRef -> {
				traitMap.put(traitRef.traitId(), new TraitEntry.Reference(traitRef));
				return;
			}

			case TubeFileEntry.TraitMethodReference traitMethodRef -> {
				methodMap.put(traitMethodRef.methodId(), new MethodEntry.Trait(traitMethodRef));
				return;
			}

			case TubeFileEntry.InstanceDefinition instanceDefinition -> {
				instanceMap.put(
					instanceDefinition.definition().instanceId(),
					new InstanceEntry.Definition(instanceDefinition)
				);
				importSpec = instanceDefinition.definition()._import();
				exportEntry = new ProgramModel.ModuleExportEntry.InstanceDefinition(instanceDefinition);
				for(var method : instanceDefinition.definition().methods()) {
					methodMap.put(
						method.methodId(),
						new MethodEntry.Instance(new TubeFileEntry.InstanceMethodReference(
							method.methodId(),
							instanceDefinition.definition().instanceId(),
							method.name(),
							method.erasedSignature(),
							method.signature()
						))
					);
				}
			}

			case TubeFileEntry.InstanceReference instanceRef -> {
				instanceMap.put(instanceRef.instanceId(), new InstanceEntry.Reference(instanceRef));
				return;
			}

			case TubeFileEntry.InstanceMethodReference instanceMethodRef -> {
				methodMap.put(instanceMethodRef.methodId(), new MethodEntry.Instance(instanceMethodRef));
				return;
			}
		}

		var moduleId = getModuleId(importSpec);
		modules
			.computeIfAbsent(moduleId, ignored -> new ModuleModelBuilder())
			.exports
			.add(exportEntry);
	}

	private static UnsignedBigInteger getModuleId(ImportSpecifier importSpec) {
		return switch(importSpec) {
			case ImportSpecifier.Global global -> global.moduleId();
			case ImportSpecifier.Local local -> getModuleId(local.parent());
		};
	}

	private static JvmPlatformTubeMetadata decodePlatformMetadata(
		Optional<ESExpr> expr
	) throws DecodeException {
		if(expr.isEmpty()) {
			return EMPTY_PLATFORM_METADATA;
		}

		return JvmPlatformTubeMetadata.codec().decode(expr.get());
	}

	private static final class ModuleModelBuilder {
		private final List<ProgramModel.ModuleExportEntry> exports = new ArrayList<>();
	}

	private sealed interface FunctionEntry {
		record Definition(TubeFileEntry.FunctionDefinition entry) implements FunctionEntry {
		}

		record Reference(TubeFileEntry.FunctionReference entry) implements FunctionEntry {
		}
	}

	private sealed interface RecordEntry {
		record Definition(TubeFileEntry.RecordDefinition entry) implements RecordEntry {
		}

		record Reference(TubeFileEntry.RecordReference entry) implements RecordEntry {
		}
	}

	private sealed interface RecordFieldEntry {
		record OfRecord(TubeFileEntry.RecordFieldReference entry) implements RecordFieldEntry {
		}

		record EnumVariant(TubeFileEntry.EnumVariantRecordFieldReference entry) implements RecordFieldEntry {
		}
	}

	private sealed interface EnumEntry {
		record Definition(TubeFileEntry.EnumDefinition entry) implements EnumEntry {
		}

		record Reference(TubeFileEntry.EnumReference entry) implements EnumEntry {
		}
	}

	private sealed interface TraitEntry {
		record Definition(TubeFileEntry.TraitDefinition entry) implements TraitEntry {
		}

		record Reference(TubeFileEntry.TraitReference entry) implements TraitEntry {
		}
	}

	private sealed interface MethodEntry {
		record RecordOwner(TubeFileEntry.RecordMethodReference entry) implements MethodEntry {}
		record EnumOwner(TubeFileEntry.EnumMethodReference entry) implements MethodEntry {}
		record EnumVariant(TubeFileEntry.EnumVariantMethodReference entry) implements MethodEntry {}
		record Trait(TubeFileEntry.TraitMethodReference entry) implements MethodEntry {
		}

		record Instance(TubeFileEntry.InstanceMethodReference entry) implements MethodEntry {
		}
	}

	private sealed interface InstanceEntry {
		record Definition(TubeFileEntry.InstanceDefinition entry) implements InstanceEntry {
		}

		record Reference(TubeFileEntry.InstanceReference entry) implements InstanceEntry {
		}
	}

	private static final class ProgramModelImpl implements ProgramModel {
		private static final String BUILDER_CLASS_NAME = "Builder";
		private static final String BUILDER_METHOD_NAME = "builder";
		private static final String BUILD_METHOD_NAME = "build";

		private final TubeHeader header;
		private final TubeMetadata metadata;
		private final DecodedMetadata decodedMetadata;
		private final List<ModuleModel> modules;
		private final Map<UnsignedBigInteger, TubeFileEntry.ModuleReference> moduleRefMap;
		private final Map<UnsignedBigInteger, FunctionEntry> functionMap;
		private final Map<UnsignedBigInteger, RecordEntry> recordMap;
		private final Map<UnsignedBigInteger, RecordFieldEntry> recordFieldMap;
		private final Map<UnsignedBigInteger, EnumEntry> enumMap;
		private final Map<UnsignedBigInteger, TubeFileEntry.EnumVariantReference> enumVariantMap;
		private final Map<UnsignedBigInteger, TraitEntry> traitMap;
		private final Map<UnsignedBigInteger, MethodEntry> methodMap;
		private final Map<UnsignedBigInteger, InstanceEntry> instanceMap;

		private final ConcurrentMap<UnsignedBigInteger, TubeInfo> tubeInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, ModuleInfo> moduleInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, FunctionInfo> functionInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, RecordInfo> recordInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, RecordFieldInfo> recordFieldInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, EnumInfo> enumInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, EnumVariantInfo> enumVariantInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, TraitInfo> traitInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, MethodInfo> methodInfoCache = new ConcurrentHashMap<>();
		private final ConcurrentMap<UnsignedBigInteger, InstanceInfo> instanceInfoCache = new ConcurrentHashMap<>();

		private ProgramModelImpl(
			TubeHeader header,
			TubeMetadata metadata,
			DecodedMetadata decodedMetadata,
			List<ModuleModel> modules,
			Map<UnsignedBigInteger, TubeFileEntry.ModuleReference> moduleRefMap,
			Map<UnsignedBigInteger, FunctionEntry> functionMap,
			Map<UnsignedBigInteger, RecordEntry> recordMap,
			Map<UnsignedBigInteger, RecordFieldEntry> recordFieldMap,
			Map<UnsignedBigInteger, EnumEntry> enumMap,
			Map<UnsignedBigInteger, TubeFileEntry.EnumVariantReference> enumVariantMap,
			Map<UnsignedBigInteger, TraitEntry> traitMap,
			Map<UnsignedBigInteger, MethodEntry> methodMap,
			Map<UnsignedBigInteger, InstanceEntry> instanceMap
		) {
			this.header = header;
			this.metadata = metadata;
			this.decodedMetadata = decodedMetadata;
			this.modules = modules;
			this.moduleRefMap = moduleRefMap;
			this.functionMap = functionMap;
			this.recordMap = recordMap;
			this.recordFieldMap = recordFieldMap;
			this.enumMap = enumMap;
			this.enumVariantMap = enumVariantMap;
			this.traitMap = traitMap;
			this.methodMap = methodMap;
			this.instanceMap = instanceMap;
		}

		@Override
		public TubeHeader header() {
			return header;
		}

		@Override
		public TubeMetadata metadata() {
			return metadata;
		}

		@Override
		public DecodedMetadata decodedMetadata() {
			return decodedMetadata;
		}

		@Override
		public List<ModuleModel> modules() {
			return modules;
		}

		@Override
		public TubeInfo getTubeInfo(UnsignedBigInteger id) {
			return tubeInfoCache.computeIfAbsent(id, this::computeTubeInfo);
		}

		private TubeInfo computeTubeInfo(UnsignedBigInteger id) {
			if(id.equals(UnsignedBigInteger.ZERO)) {
				return new TubeInfo(
					metadata.name(),
					decodedMetadata.platformMetadata(),
					ClassNaming.tubeModuleName(metadata.name(), decodedMetadata.platformMetadata())
				);
			}

			var tubeRefIndexBigInt = id.toBigInteger().subtract(BigInteger.ONE);
			if(
				tubeRefIndexBigInt.signum() < 0 ||
					tubeRefIndexBigInt.compareTo(BigInteger.valueOf(metadata.referencedTubes().size())) >= 0
			) {
				throw new IllegalStateException("Invalid tube id");
			}

			var tubeRef = metadata.referencedTubes().get(tubeRefIndexBigInt.intValueExact());
			var platformMetadata = decodedMetadata.referencePlatformMetadata()
				.getOrDefault(tubeRef.name(), EMPTY_PLATFORM_METADATA);

			return new TubeInfo(
				tubeRef.name(),
				platformMetadata,
				ClassNaming.tubeModuleName(tubeRef.name(), platformMetadata)
			);
		}

		@Override
		public ModuleInfo getModuleInfo(UnsignedBigInteger id) {
			return moduleInfoCache.computeIfAbsent(id, this::computeModuleInfo);
		}

		private ModuleInfo computeModuleInfo(UnsignedBigInteger id) {
			if(id.toBigInteger().compareTo(BigInteger.valueOf(metadata.modules().size())) < 0) {
				var module = metadata.modules().get(id.toBigInteger().intValueExact());
				var packageName = PackageDesc.of(ClassNaming.tubeModulePackageName(this, module.path(), UnsignedBigInteger.ZERO));
				return new ModuleInfo(UnsignedBigInteger.ZERO, module.path(), packageName);
			}

			var moduleRef = moduleRefMap.get(id);
			if(moduleRef == null) {
				throw new IllegalStateException("Could not get referenced module: " + id);
			}

			var packageName = PackageDesc.of(ClassNaming.tubeModulePackageName(this, moduleRef.path(), moduleRef.tubeId()));
			return new ModuleInfo(moduleRef.tubeId(), moduleRef.path(), packageName);
		}

		@Override
		public FunctionInfo getFunctionInfo(UnsignedBigInteger id) {
			return functionInfoCache.computeIfAbsent(id, this::computeFunctionInfo);
		}

		private FunctionInfo computeFunctionInfo(UnsignedBigInteger id) {
			var entry = require(functionMap, id, "Invalid function id");
			var importSpecifier = functionImportSpecifier(entry);
			return new FunctionInfo(
				ClassNaming.moduleGlobalFunctionsClassName(getModuleInfo(getModuleId(importSpecifier))),
				ClassNaming.functionName(importSpecifier),
				functionSignature(entry),
				functionDescriptor(entry)
			);
		}

		@Override
		public RecordInfo getRecordInfo(UnsignedBigInteger id) {
			return recordInfoCache.computeIfAbsent(id, this::computeRecordInfo);
		}

		private RecordInfo computeRecordInfo(UnsignedBigInteger id) {
			var entry = require(recordMap, id, "Invalid record id");

			var importSpecifier = recordImportSpecifier(entry);
			var signature = recordSignature(entry);

			var classDesc = ClassNaming.typeDefinitionClassDescriptor(this, importSpecifier);
			var builderClass = classDesc.nested(BUILDER_CLASS_NAME);

			return new RecordInfo(
				importSpecifier,
				signature,
				classDesc,
				new RecordBuilderInfo(
					builderClass,
					BUILDER_METHOD_NAME,
					MethodTypeDesc.of(
						builderClass,
						signature.tokenParameters().stream()
							.map(parameter -> tokenClassDesc(parameter.kind()))
							.toArray(ClassDesc[]::new)
					),
					BUILD_METHOD_NAME,
					MethodTypeDesc.of(classDesc)
				)
			);
		}

		@Override
		public RecordFieldInfo getRecordFieldInfo(UnsignedBigInteger id) {
			return recordFieldInfoCache.computeIfAbsent(id, this::computeRecordFieldInfo);
		}

		private RecordFieldInfo computeRecordFieldInfo(UnsignedBigInteger id) {
			return switch(require(recordFieldMap, id, "Invalid record field id")) {
				case RecordFieldEntry.OfRecord(var entry) -> {
					var fieldType = tokenClassDesc(entry.fieldType());
					var fieldName = ClassNaming.fieldName(entry.name());
					yield new RecordFieldInfo(
						RecordFieldInfo.OwnerType.RECORD,
						entry.recordId(),
						entry.name(),
						fieldName,
						fieldType,
						"set_" + fieldName,
						MethodTypeDesc.of(
							getRecordInfo(entry.recordId()).builderInfo().builderClassDesc(),
							fieldType
						)
					);
				}

				case RecordFieldEntry.EnumVariant(var entry) -> {
					var fieldType = tokenClassDesc(entry.fieldType());
					var fieldName = ClassNaming.fieldName(entry.name());
					yield new RecordFieldInfo(
						RecordFieldInfo.OwnerType.ENUM_VARIANT,
						entry.variantId(),
						entry.name(),
						fieldName,
						fieldType,
						"set_" + fieldName,
						MethodTypeDesc.of(
							getEnumVariantInfo(entry.variantId()).builder().builderClassDesc(),
							fieldType
						)
					);
				}
			};
		}

		@Override
		public EnumInfo getEnumInfo(UnsignedBigInteger id) {
			return enumInfoCache.computeIfAbsent(id, this::computeEnumInfo);
		}

		private EnumInfo computeEnumInfo(UnsignedBigInteger id) {
			var importSpecifier = enumImportSpecifier(require(enumMap, id, "Invalid enum id"));
			var classDesc = ClassNaming.typeDefinitionClassDescriptor(this, importSpecifier);

			return new EnumInfo(
				importSpecifier,
				classDesc
			);
		}

		@Override
		public EnumVariantInfo getEnumVariantInfo(UnsignedBigInteger id) {
			return enumVariantInfoCache.computeIfAbsent(id, this::computeEnumVariantInfo);
		}

		private EnumVariantInfo computeEnumVariantInfo(UnsignedBigInteger id) {
			var entry = require(enumVariantMap, id, "Invalid enum variant id");
			var enumInfo = getEnumInfo(entry.enumId());
			var variantName = ClassNaming.variantName(entry.name());
			var variantClassDesc = enumInfo.enumClassDesc().nested(variantName);
			var builderClass = variantClassDesc.nested(BUILDER_CLASS_NAME);

			var parameterTypes = new ArrayList<ClassDesc>();
			parameterTypes.addAll(enumSignature(require(enumMap, entry.enumId(), "Invalid enum id")).tokenParameters().stream()
				.map(parameter -> tokenClassDesc(parameter.kind()))
				.toList());
			parameterTypes.addAll(signatureParameterDescs(entry.signature()));

			return new EnumVariantInfo(
				entry.enumId(),
				entry.name(),
				entry.signature(),
				variantName,
				variantClassDesc,
				new RecordBuilderInfo(
					builderClass,
					BUILDER_METHOD_NAME,
					MethodTypeDesc.of(builderClass, parameterTypes),
					BUILD_METHOD_NAME,
					MethodTypeDesc.of(variantClassDesc)
				)
			);
		}

		@Override
		public TraitInfo getTraitInfo(UnsignedBigInteger id) {
			return traitInfoCache.computeIfAbsent(id, this::computeTraitInfo);
		}

		private TraitInfo computeTraitInfo(UnsignedBigInteger id) {
			var entry = require(traitMap, id, "Invalid trait id");
			var classDesc = ClassNaming.typeDefinitionClassDescriptor(this, traitImportSpecifier(entry));

			return new TraitInfo(
				traitImportSpecifier(entry),
				traitSignature(entry),
				classDesc
			);
		}

		@Override
		public MethodInfo getMethodInfo(UnsignedBigInteger id) {
			return methodInfoCache.computeIfAbsent(id, this::computeMethodInfo);
		}

		private MethodInfo computeMethodInfo(UnsignedBigInteger id) {
			return switch(require(methodMap, id, "Invalid method id")) {
				case MethodEntry.RecordOwner(var ref) -> methodInfo(ref.name(), ref.erasedSignature(), ref.signature(), getRecordInfo(ref.recordId()).recordClassDesc());
				case MethodEntry.EnumOwner(var ref) -> methodInfo(ref.name(), ref.erasedSignature(), ref.signature(), getEnumInfo(ref.enumId()).enumClassDesc());
				case MethodEntry.EnumVariant(var ref) -> methodInfo(ref.name(), ref.erasedSignature(), ref.signature(), getEnumVariantInfo(ref.variantId()).variantClassDesc());
				case MethodEntry.Trait(var methodRef) -> {
					var traitInfo = getTraitInfo(methodRef.traitId());

					yield new MethodInfo(
						methodRef.name(),
						methodRef.erasedSignature(),
						methodRef.signature(),
						traitInfo.traitDesc(),
						ClassNaming.methodName(methodRef.name(), methodRef.erasedSignature()),
						functionSignatureDescriptor(methodRef.signature())
					);
				}

				case MethodEntry.Instance(var methodRef) -> {
					var instanceInfo = getInstanceInfo(methodRef.instanceId());

					yield new MethodInfo(
						methodRef.name(),
						methodRef.erasedSignature(),
						methodRef.signature(),
						instanceInfo.instanceClassDesc(),
						ClassNaming.methodName(methodRef.name(), methodRef.erasedSignature()),
						functionSignatureDescriptor(methodRef.signature())
					);
				}
			};
		}

		private MethodInfo methodInfo(Identifier name, ErasedSignature erasedSignature, FunctionSignature signature, ClassDesc definingClass) {
			return new MethodInfo(name, erasedSignature, signature, definingClass,
				ClassNaming.methodName(name, erasedSignature), functionSignatureDescriptor(signature));
		}

		@Override
		public InstanceInfo getInstanceInfo(UnsignedBigInteger id) {
			return instanceInfoCache.computeIfAbsent(id, this::computeInstanceInfo);
		}

		private InstanceInfo computeInstanceInfo(UnsignedBigInteger id) {
			var entry = require(instanceMap, id, "Invalid instance id");

			var importSpecifier = instanceImportSpecifier(entry);
			var signature = instanceSignature(entry);

			return new InstanceInfo(
				importSpecifier,
				signature,
				ClassNaming.typeDefinitionClassDescriptor(this, importSpecifier),
				MethodTypeDesc.of(
					ConstantDescs.CD_void,
					signatureParameterDescs(signature)
				)
			);
		}

		private static <T> T require(Map<UnsignedBigInteger, T> map, UnsignedBigInteger id, String message) {
			var value = map.get(id);
			if(value == null) {
				throw new IllegalStateException(message);
			}

			return value;
		}

		private static ImportSpecifier functionImportSpecifier(FunctionEntry entry) {
			return switch(entry) {
				case FunctionEntry.Definition definition -> definition.entry().definition()._import();
				case FunctionEntry.Reference reference -> reference.entry()._import();
			};
		}

		private MethodTypeDesc functionDescriptor(FunctionEntry entry) {
			return functionSignatureDescriptor(functionSignature(entry));
		}

		private FunctionSignature functionSignature(FunctionEntry entry) {
			return switch(entry) {
				case FunctionEntry.Definition definition -> definition.entry().definition().signature();
				case FunctionEntry.Reference reference -> reference.entry().signature();
			};
		}

		private MethodTypeDesc functionSignatureDescriptor(FunctionSignature signature) {
			var parameterTypes = new ArrayList<ClassDesc>();
			for(var parameter : signature.tokenParameters()) {
				parameterTypes.add(tokenClassDesc(parameter.kind()));
			}
			for(var parameter : signature.parameters()) {
				parameterTypes.add(tokenClassDesc(parameter.paramType()));
			}

			return MethodTypeDesc.of(ClassDesc.of("dev.argon.runtime.Trampoline"), parameterTypes);
		}

		private ClassDesc tokenClassDesc(Token token) {
			return TokenTypes.tokenAsClassDesc(this, token);
		}

		private List<ClassDesc> signatureParameterDescs(FunctionSignature signature) {
			var parameterDescs = new ArrayList<ClassDesc>();
			for(var parameter : signature.tokenParameters()) {
				parameterDescs.add(tokenClassDesc(parameter.kind()));
			}
			for(var parameter : signature.parameters()) {
				parameterDescs.add(tokenClassDesc(parameter.paramType()));
			}
			return parameterDescs;
		}

		private static ImportSpecifier recordImportSpecifier(RecordEntry entry) {
			return switch(entry) {
				case RecordEntry.Definition definition -> definition.entry().definition()._import();
				case RecordEntry.Reference reference -> reference.entry()._import();
			};
		}

		private static FunctionSignature recordSignature(RecordEntry entry) {
			return switch(entry) {
				case RecordEntry.Definition definition -> definition.entry().definition().signature();
				case RecordEntry.Reference reference -> reference.entry().signature();
			};
		}

		private static ImportSpecifier enumImportSpecifier(EnumEntry entry) {
			return switch(entry) {
				case EnumEntry.Definition definition -> definition.entry().definition()._import();
				case EnumEntry.Reference reference -> reference.entry()._import();
			};
		}

		private static FunctionSignature enumSignature(EnumEntry entry) {
			return switch(entry) {
				case EnumEntry.Definition definition -> definition.entry().definition().signature();
				case EnumEntry.Reference reference -> reference.entry().signature();
			};
		}

		private static ImportSpecifier traitImportSpecifier(TraitEntry entry) {
			return switch(entry) {
				case TraitEntry.Definition definition -> definition.entry().definition()._import();
				case TraitEntry.Reference reference -> reference.entry()._import();
			};
		}

		private static FunctionSignature traitSignature(TraitEntry entry) {
			return switch(entry) {
				case TraitEntry.Definition definition -> definition.entry().definition().signature();
				case TraitEntry.Reference reference -> reference.entry().signature();
			};
		}

		private static ImportSpecifier instanceImportSpecifier(InstanceEntry entry) {
			return switch(entry) {
				case InstanceEntry.Definition definition -> definition.entry().definition()._import();
				case InstanceEntry.Reference reference -> reference.entry()._import();
			};
		}

		private static FunctionSignature instanceSignature(InstanceEntry entry) {
			return switch(entry) {
				case InstanceEntry.Definition definition -> definition.entry().definition().signature();
				case InstanceEntry.Reference reference -> reference.entry().signature();
			};
		}
	}
}
