package dev.argon.backend.codegen;

import com.google.common.collect.ImmutableMap;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.*;

import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.util.List;
import java.util.Optional;

interface ProgramModel {
	TubeHeader header();
	TubeMetadata metadata();
	DecodedMetadata decodedMetadata();
	List<ModuleModel> modules();

	TubeInfo getTubeInfo(UnsignedBigInteger id);
	ModuleInfo getModuleInfo(UnsignedBigInteger id);
	FunctionInfo getFunctionInfo(UnsignedBigInteger id);
	RecordInfo getRecordInfo(UnsignedBigInteger id);
	RecordFieldInfo getRecordFieldInfo(UnsignedBigInteger id);
	EnumInfo getEnumInfo(UnsignedBigInteger id);
	EnumVariantInfo getEnumVariantInfo(UnsignedBigInteger id);
	TraitInfo getTraitInfo(UnsignedBigInteger id);
	MethodInfo getMethodInfo(UnsignedBigInteger id);
	InstanceInfo getInstanceInfo(UnsignedBigInteger id);





	record DecodedMetadata(
		JvmPlatformTubeMetadata platformMetadata,
		ImmutableMap<TubeName, JvmPlatformTubeMetadata> referencePlatformMetadata
	) {
	}

	record ModuleModel(
		ModulePath path,
		List<ModuleExportEntry> exports
	) {
	}

	sealed interface ModuleExportEntry {
		TubeFileEntry entry();

		record FunctionDefinition(TubeFileEntry.FunctionDefinition entry) implements ModuleExportEntry {
		}

		record RecordDefinition(TubeFileEntry.RecordDefinition entry) implements ModuleExportEntry {
		}

		record EnumDefinition(TubeFileEntry.EnumDefinition entry) implements ModuleExportEntry {
		}

		record TraitDefinition(TubeFileEntry.TraitDefinition entry) implements ModuleExportEntry {
		}

		record InstanceDefinition(TubeFileEntry.InstanceDefinition entry) implements ModuleExportEntry {
		}
	}

	record TubeInfo(
		TubeName tubeName,
		JvmPlatformTubeMetadata platformMetadata
	) {
	}

	record ModuleInfo(
		UnsignedBigInteger tubeId,
		ModulePath path
	) {
	}

	record FunctionInfo(
		ClassDesc definingClass,
		String name,
		FunctionSignature signature,
		MethodTypeDesc descriptor
	) {
	}

	record RecordInfo(
		ImportSpecifier importSpecifier,
		FunctionSignature signature
	) {
	}

	record RecordFieldInfo(
		OwnerType ownerType,
		UnsignedBigInteger recordId,
		Identifier name
	) {
		public enum OwnerType {
			RECORD,
			ENUM_VARIANT,
		}
	}

	record EnumInfo(
		ImportSpecifier importSpecifier
	) {
	}

	record EnumVariantInfo(
		UnsignedBigInteger enumId,
		Identifier name
	) {
	}

	record TraitInfo(
		ImportSpecifier importSpecifier,
		FunctionSignature signature
	) {
	}

	record MethodInfo(
		ImportSpecifier parentImportSpecifier,
		Identifier name,
		ErasedSignature erasedSignature,
		FunctionSignature signature,
		Optional<dev.argon.vm.MethodDefinition> definition
	) {
	}

	record InstanceInfo(
		ImportSpecifier importSpecifier,
		FunctionSignature signature
	) {
	}
}
