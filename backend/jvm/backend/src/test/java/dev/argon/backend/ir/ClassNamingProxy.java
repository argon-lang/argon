package dev.argon.backend.ir;

import dev.argon.backend.codegen.ProgramModel;
import dev.argon.esexpr.UnsignedBigInteger;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.ErasedSignature;
import dev.argon.vm.Identifier;
import dev.argon.vm.ImportSpecifier;
import dev.argon.vm.ModulePath;
import dev.argon.vm.TubeName;

import java.lang.constant.ClassDesc;
import java.lang.constant.ModuleDesc;

public final class ClassNamingProxy {
	private ClassNamingProxy() {
	}

	public static ModuleDesc tubeModuleName(TubeName tubeName, JvmPlatformTubeMetadata platformMetadata) {
		return ClassNaming.tubeModuleName(tubeName, platformMetadata);
	}

	public static String tubeModulePackageName(ProgramModel program, ModulePath modulePath, UnsignedBigInteger tubeId) {
		return ClassNaming.tubeModulePackageName(program, modulePath, tubeId);
	}

	public static ClassDesc typeDefinitionClassDescriptor(ProgramModel program, ImportSpecifier importSpecifier) {
		return ClassNaming.typeDefinitionClassDescriptor(program, importSpecifier);
	}

	public static String functionName(ImportSpecifier importSpecifier) {
		return ClassNaming.functionName(importSpecifier);
	}

	public static String fieldName(Identifier identifier) {
		return ClassNaming.fieldName(identifier);
	}

	public static String variantName(Identifier identifier) {
		return ClassNaming.variantName(identifier);
	}

	public static String methodName(Identifier identifier, ErasedSignature signature) {
		return ClassNaming.methodName(identifier, signature);
	}

	public static String defaultTubeModuleName(TubeName tubeName) {
		return ClassNaming.defaultTubeModuleName(tubeName);
	}

	public static String defaultModulePackageName(TubeName tubeName, ModulePath modulePath) {
		return ClassNaming.defaultModulePackageName(tubeName, modulePath);
	}
}
