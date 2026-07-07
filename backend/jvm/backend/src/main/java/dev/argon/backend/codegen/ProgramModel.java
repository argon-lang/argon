package dev.argon.backend.codegen;

import com.google.common.collect.ImmutableMap;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.*;

import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.constant.ModuleDesc;
import java.lang.constant.PackageDesc;
import java.util.List;

public interface ProgramModel {
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
		List<ModuleExportEntry> exports,
		UnsignedBigInteger moduleId
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
		JvmPlatformTubeMetadata platformMetadata,
		ModuleDesc moduleName
	) {
	}

	record ModuleInfo(
		UnsignedBigInteger tubeId,
		ModulePath path,
		PackageDesc packageName
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
		FunctionSignature signature,
		ClassDesc recordClassDesc,
		RecordBuilderInfo builderInfo
	) {
	}

	record RecordBuilderInfo(
		ClassDesc builderClassDesc,
		String builderMethodName,
		MethodTypeDesc builderFactoryMethodDesc,
		String buildMethodName,
		MethodTypeDesc buildMethodDesc
	) {}

	record RecordFieldInfo(
		OwnerType ownerType,
		UnsignedBigInteger recordId,
		Identifier name,
		String fieldName,
		ClassDesc fieldType,
		String builderMethodName,
		MethodTypeDesc builderMethodDesc
	) {
		public enum OwnerType {
			RECORD,
			ENUM_VARIANT,
		}
	}

	record EnumInfo(
		ImportSpecifier importSpecifier,
		ClassDesc enumClassDesc
	) {
	}

	record EnumVariantInfo(
		UnsignedBigInteger enumId,
		Identifier name,
		String nestedClassName,
		ClassDesc variantClassDesc,
		RecordBuilderInfo builder
	) {
	}

	record TraitInfo(
		ImportSpecifier importSpecifier,
		FunctionSignature signature,
		ClassDesc traitDesc
	) {
	}

	record MethodInfo(
		Identifier name,
		ErasedSignature erasedSignature,
		FunctionSignature signature,
		ClassDesc definingClass,
		String methodName,
		MethodTypeDesc descriptor
	) {
	}

	record InstanceInfo(
		ImportSpecifier importSpecifier,
		FunctionSignature signature,
		ClassDesc instanceClassDesc,
		MethodTypeDesc instanceConstructorDesc
	) {
	}
}
