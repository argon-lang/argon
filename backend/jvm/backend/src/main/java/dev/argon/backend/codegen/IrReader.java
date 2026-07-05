package dev.argon.backend.codegen;

import com.google.common.collect.ImmutableMap;
import dev.argon.backend.io.InputFile;
import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.ESExpr;
import dev.argon.esexpr.ESExprBinaryReader;
import dev.argon.esexpr.SyntaxException;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.*;

import java.io.IOException;
import java.io.InputStream;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

final class IrReader {
	private static final dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata EMPTY_PLATFORM_METADATA =
		new dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata(
			Optional.empty(),
			Optional.empty(),
			Optional.empty()
		);

	private TubeHeader header;
	private TubeMetadata metadata;
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

			moduleModels.add(new ProgramModel.ModuleModel(declaredModules.get(i).path(), exports));
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
			case TubeFileEntry.Header ignored ->
				throw new IllegalStateException("Extra header not allowed in VMIR");

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
			}

			case TubeFileEntry.RecordReference recordRef -> {
				recordMap.put(recordRef.recordId(), new RecordEntry.Reference(recordRef));
				return;
			}

			case TubeFileEntry.EnumDefinition enumDefinition -> {
				enumMap.put(
					enumDefinition.definition().enumId(),
					new EnumEntry.Definition(enumDefinition)
				);
				importSpec = enumDefinition.definition()._import();
				exportEntry = new ProgramModel.ModuleExportEntry.EnumDefinition(enumDefinition);
			}

			case TubeFileEntry.EnumReference enumRef -> {
				enumMap.put(enumRef.enumId(), new EnumEntry.Reference(enumRef));
				return;
			}

			case TubeFileEntry.EnumVariantReference enumVariantRef -> {
				enumVariantMap.put(enumVariantRef.variantId(), enumVariantRef);
				return;
			}

			case TubeFileEntry.RecordFieldReference recordFieldRef -> {
				recordFieldMap.put(
					recordFieldRef.recordFieldId(),
					new RecordFieldEntry.Record(recordFieldRef)
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

		return dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata.codec().decode(expr.get());
	}

	private static final class ModuleModelBuilder {
		private final List<ProgramModel.ModuleExportEntry> exports = new ArrayList<>();
	}

	private sealed interface FunctionEntry {
		record Definition(TubeFileEntry.FunctionDefinition entry) implements FunctionEntry {}
		record Reference(TubeFileEntry.FunctionReference entry) implements FunctionEntry {}
	}

	private sealed interface RecordEntry {
		record Definition(TubeFileEntry.RecordDefinition entry) implements RecordEntry {}
		record Reference(TubeFileEntry.RecordReference entry) implements RecordEntry {}
	}

	private sealed interface RecordFieldEntry {
		record Record(TubeFileEntry.RecordFieldReference entry) implements RecordFieldEntry {}
		record EnumVariant(TubeFileEntry.EnumVariantRecordFieldReference entry) implements RecordFieldEntry {}
	}

	private sealed interface EnumEntry {
		record Definition(TubeFileEntry.EnumDefinition entry) implements EnumEntry {}
		record Reference(TubeFileEntry.EnumReference entry) implements EnumEntry {}
	}

	private sealed interface TraitEntry {
		record Definition(TubeFileEntry.TraitDefinition entry) implements TraitEntry {}
		record Reference(TubeFileEntry.TraitReference entry) implements TraitEntry {}
	}

	private sealed interface MethodEntry {
		record Trait(TubeFileEntry.TraitMethodReference entry) implements MethodEntry {}
		record Instance(TubeFileEntry.InstanceMethodReference entry) implements MethodEntry {}
	}

	private sealed interface InstanceEntry {
		record Definition(TubeFileEntry.InstanceDefinition entry) implements InstanceEntry {}
		record Reference(TubeFileEntry.InstanceReference entry) implements InstanceEntry {}
	}

	private record ProgramModelImpl(
		TubeHeader header,
		TubeMetadata metadata,
		ProgramModel.DecodedMetadata decodedMetadata,
		List<ProgramModel.ModuleModel> modules,
		Map<UnsignedBigInteger, TubeFileEntry.ModuleReference> moduleRefMap,
		Map<UnsignedBigInteger, FunctionEntry> functionMap,
		Map<UnsignedBigInteger, RecordEntry> recordMap,
		Map<UnsignedBigInteger, RecordFieldEntry> recordFieldMap,
		Map<UnsignedBigInteger, EnumEntry> enumMap,
		Map<UnsignedBigInteger, TubeFileEntry.EnumVariantReference> enumVariantMap,
		Map<UnsignedBigInteger, TraitEntry> traitMap,
		Map<UnsignedBigInteger, MethodEntry> methodMap,
		Map<UnsignedBigInteger, InstanceEntry> instanceMap
	) implements ProgramModel {
		@Override
		public ProgramModel.TubeInfo getTubeInfo(UnsignedBigInteger id) {
			if(id.equals(UnsignedBigInteger.ZERO)) {
				return new ProgramModel.TubeInfo(metadata.name(), decodedMetadata.platformMetadata());
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

			return new ProgramModel.TubeInfo(tubeRef.name(), platformMetadata);
		}

		@Override
		public ProgramModel.ModuleInfo getModuleInfo(UnsignedBigInteger id) {
			if(id.toBigInteger().compareTo(BigInteger.valueOf(metadata.modules().size())) < 0) {
				var module = metadata.modules().get(id.toBigInteger().intValueExact());
				return new ProgramModel.ModuleInfo(UnsignedBigInteger.ZERO, module.path());
			}

			var moduleRef = moduleRefMap.get(id);
			if(moduleRef == null) {
				throw new IllegalStateException("Could not get referenced module: " + id);
			}

			return new ProgramModel.ModuleInfo(moduleRef.tubeId(), moduleRef.path());
		}

		@Override
		public ProgramModel.FunctionInfo getFunctionInfo(UnsignedBigInteger id) {
			var entry = require(functionMap, id, "Invalid function id");
			var importSpecifier = functionImportSpecifier(entry);
				return new ProgramModel.FunctionInfo(
					ClassNaming.moduleGlobalFunctionsClassName(this, getModuleInfo(getModuleId(importSpecifier))),
					ClassNaming.functionName(importSpecifier),
					functionSignature(entry),
					functionDescriptor(entry)
				);
			}

			@Override
			public ProgramModel.RecordInfo getRecordInfo(UnsignedBigInteger id) {
				var entry = require(recordMap, id, "Invalid record id");
				return new ProgramModel.RecordInfo(recordImportSpecifier(entry), recordSignature(entry));
			}

		@Override
		public ProgramModel.RecordFieldInfo getRecordFieldInfo(UnsignedBigInteger id) {
			return switch(require(recordFieldMap, id, "Invalid record field id")) {
				case RecordFieldEntry.Record fieldEntry ->
					new ProgramModel.RecordFieldInfo(
						ProgramModel.RecordFieldInfo.OwnerType.RECORD,
						fieldEntry.entry().recordId(),
						fieldEntry.entry().name()
					);

				case RecordFieldEntry.EnumVariant fieldEntry ->
					new ProgramModel.RecordFieldInfo(
						ProgramModel.RecordFieldInfo.OwnerType.ENUM_VARIANT,
						fieldEntry.entry().variantId(),
						fieldEntry.entry().name()
					);
			};
		}

		@Override
		public ProgramModel.EnumInfo getEnumInfo(UnsignedBigInteger id) {
			return new ProgramModel.EnumInfo(enumImportSpecifier(require(enumMap, id, "Invalid enum id")));
		}

		@Override
		public ProgramModel.EnumVariantInfo getEnumVariantInfo(UnsignedBigInteger id) {
			var entry = require(enumVariantMap, id, "Invalid enum variant id");
			return new ProgramModel.EnumVariantInfo(entry.enumId(), entry.name());
		}

		@Override
		public ProgramModel.TraitInfo getTraitInfo(UnsignedBigInteger id) {
			var entry = require(traitMap, id, "Invalid trait id");
			return new ProgramModel.TraitInfo(traitImportSpecifier(entry), traitSignature(entry));
		}

		@Override
		public ProgramModel.MethodInfo getMethodInfo(UnsignedBigInteger id) {
			return switch(require(methodMap, id, "Invalid method id")) {
				case MethodEntry.Trait methodEntry -> {
					var methodRef = methodEntry.entry();
					var traitInfo = getTraitInfo(methodRef.traitId());
					yield new ProgramModel.MethodInfo(
							traitInfo.importSpecifier(),
							methodRef.name(),
							methodRef.erasedSignature(),
							methodRef.signature(),
							Optional.empty()
						);
					}

				case MethodEntry.Instance methodEntry -> {
					var methodRef = methodEntry.entry();
					var instanceInfo = getInstanceInfo(methodRef.instanceId());
					yield new ProgramModel.MethodInfo(
							instanceInfo.importSpecifier(),
							methodRef.name(),
							methodRef.erasedSignature(),
							methodRef.signature(),
							Optional.empty()
						);
					}
			};
		}

		@Override
		public ProgramModel.InstanceInfo getInstanceInfo(UnsignedBigInteger id) {
			var entry = require(instanceMap, id, "Invalid instance id");
			return new ProgramModel.InstanceInfo(instanceImportSpecifier(entry), instanceSignature(entry));
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
			return switch(token) {
				case Token.Boxed _ -> ConstantDescs.CD_Object;
				case Token.Builtin(var bt) -> switch(bt) {
					case BuiltinType.Array(var elementType) -> tokenClassDesc(elementType).arrayType();
					case BuiltinType.Bool() -> ConstantDescs.CD_boolean;
					case BuiltinType.Conjunction _ -> throw new RuntimeException("Conjunction not implemented");
					case BuiltinType.Disjunction _ -> throw new RuntimeException("Disjunction not implemented");
					case BuiltinType.Int() -> ClassDesc.of("java.math.BigInteger");
					case BuiltinType.Never() -> ClassDesc.of("dev.argon.runtime.Never");
					case BuiltinType.String() -> ConstantDescs.CD_String;
				};
				case Token.Enum enumToken ->
					ClassNaming.typeDefinitionClassDescriptor(this, getEnumInfo(enumToken.enumId()).importSpecifier());
				case Token.Function _ -> ClassDesc.of("dev.argon.runtime.Function");
				case Token.FunctionErased _ -> ClassDesc.of("dev.argon.runtime.FunctionErased");
				case Token.FunctionToken _ -> ClassDesc.of("dev.argon.runtime.FunctionToken");
				case Token.InstanceType instanceType ->
					ClassNaming.typeDefinitionClassDescriptor(this, getInstanceInfo(instanceType.instanceId()).importSpecifier());
				case Token.InstanceValue _ -> throw new UnsupportedOperationException("InstanceValue cannot be used as a type");
				case Token.ParentTokenParameter _, Token.TokenParameter _ -> ConstantDescs.CD_Object;
				case Token.Record record ->
					ClassNaming.typeDefinitionClassDescriptor(this, getRecordInfo(record.recordId()).importSpecifier());
				case Token.RefCell _ -> ClassDesc.of("dev.argon.runtime.RefCell");
				case Token.Trait trait ->
					ClassNaming.typeDefinitionClassDescriptor(this, getTraitInfo(trait.traitId()).importSpecifier());
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
