package dev.argon.backend.externs;

import com.google.common.collect.ImmutableMap;
import dev.argon.backend.api.Extern;
import dev.argon.backend.api.PlatformMetadataResult;
import dev.argon.backend.io.InputFile;
import dev.argon.esexpr.KeywordMapping;
import dev.argon.jvmbackendmetadata.Classfile;
import dev.argon.jvmbackendmetadata.JvmExtern;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.jvmbackendmetadata.ModuleMetadata;
import dev.argon.vm.ModulePath;

import java.io.IOException;
import java.lang.classfile.Annotation;
import java.lang.classfile.AnnotationValue;
import java.lang.classfile.Attributes;
import java.lang.classfile.ClassModel;
import java.lang.classfile.AttributedElement;
import java.lang.classfile.MethodModel;
import java.lang.reflect.AccessFlag;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

public final class ExternMetadataScanner {
	private static final String EXTERN_FUNCTION_DESCRIPTOR = "Ldev/argon/runtime/ExternFunction;";
	private static final String EXTERN_METHOD_DESCRIPTOR = "Ldev/argon/runtime/ExternMethod;";
	private static final String EXTERN_STATIC_METHOD_DESCRIPTOR = "Ldev/argon/runtime/ExternStaticMethod;";
	private static final String ARGON_MODULE_DESCRIPTOR = "Ldev/argon/runtime/ArgonModule;";

	private ExternMetadataScanner() {
	}

	public static PlatformMetadataResult platformMetadata(List<InputFile> externClassfiles) throws IOException {
		var classfiles = loadClassfiles(externClassfiles);
		var metadata = new MetadataBuilder();

		for(var classfile : classfiles) {
			var classModel = classfile.model();
			if(classModel.isModuleInfo()) {
				processModuleInfo(metadata, classModel);
			}
			else if(isPackageInfo(classModel)) {
				processPackageInfo(metadata, classModel);
			}
			else {
				processClass(metadata, classModel);
			}
		}

		var tubeMetadata = new JvmPlatformTubeMetadata(metadata.moduleName,
			metadata.moduleMetadata.isEmpty() ? Optional.empty() : Optional.of(List.copyOf(metadata.moduleMetadata)),
			Optional.of(classfiles));

		return new PlatformMetadataResult("jvm", JvmPlatformTubeMetadata.codec().encode(tubeMetadata),
			new KeywordMapping<>(metadata.externs.build()));
	}

	private static void processModuleInfo(MetadataBuilder metadata, ClassModel classModel) {
		var moduleInfoName = moduleName(classModel);
		if(metadata.moduleName.isPresent() && !metadata.moduleName.get().equals(moduleInfoName)) {
			throw new IllegalArgumentException(
				"Conflicting JVM module names: " + metadata.moduleName.get() + " and " + moduleInfoName);
		}

		metadata.moduleName = Optional.of(moduleInfoName);
	}

	private static void processPackageInfo(MetadataBuilder metadata, ClassModel classModel) {
		var className = classModel.thisClass().asInternalName();
		for(var annotation : annotations(classModel)) {
			if(!annotation.classSymbol().descriptorString().equals(ARGON_MODULE_DESCRIPTOR)) {
				continue;
			}

			metadata.moduleMetadata.add(new ModuleMetadata(new ModulePath(argonModulePath(annotation)),
				Optional.of(packageNameFromPackageInfoClass(className))));
			return;
		}
	}

	private static void processClass(MetadataBuilder metadata, ClassModel classModel) {
		for(var method : classModel.methods()) {
			var externAnnotation = externAnnotation(method);
			if(externAnnotation.isEmpty()) {
				continue;
			}
			var externName = externAnnotation.get().name();

			validateExternMethod(classModel, method, externName);

			if(!metadata.externNames.add(externName)) {
				throw new IllegalArgumentException("Duplicate extern: " + externName);
			}

			var implementation = new JvmExtern.JvmFunction(classModel.thisClass().name().stringValue(),
				method.methodName().stringValue(), method.methodType().stringValue());

			var encoded = JvmExtern.codec().encode(implementation);
			Extern extern = switch(externAnnotation.get().kind()) {
				case FUNCTION -> new Extern.ExternFunction(externName, encoded);
				case METHOD -> new Extern.ExternMethod(externName, encoded);
				case STATIC_METHOD -> new Extern.ExternStaticMethod(externName, encoded);
			};
			metadata.externs.put(externName, extern);
		}
	}

	private static final class MetadataBuilder {
		private final ImmutableMap.Builder<String, Extern> externs = ImmutableMap.builder();
		private final HashSet<String> externNames = new HashSet<>();
		private Optional<String> moduleName = Optional.empty();
		private final ArrayList<ModuleMetadata> moduleMetadata = new ArrayList<>();
	}

	private static List<Classfile> loadClassfiles(List<InputFile> files) throws IOException {
		var classfiles = new ArrayList<Classfile>();

		for(var file : files) {
			try(var input = file.open()) {
				classfiles.add(Classfile.parse(input.readAllBytes()));
			}
			catch(IllegalArgumentException ex) {
				throw new IllegalArgumentException("Invalid classfile " + file.fileName() + ": " + ex.getMessage(), ex);
			}
		}

		return List.copyOf(classfiles);
	}

	private static String moduleName(ClassModel classModel) {
		return classModel.findAttribute(Attributes.module())
			.orElseThrow(() -> new IllegalArgumentException("module-info classfile is missing its Module attribute"))
			.moduleName().name().stringValue();
	}

	private static boolean isPackageInfo(ClassModel classModel) {
		var className = classModel.thisClass().asInternalName();
		return className.equals("package-info") || className.endsWith("/package-info");
	}

	private static String packageNameFromPackageInfoClass(String className) {
		var packageInternalName = className.equals("package-info")
			? ""
			: className.substring(0, className.length() - "/package-info".length());

		return packageInternalName.replace('/', '.');
	}

	private static List<String> argonModulePath(Annotation annotation) {
		for(var element : annotation.elements()) {
			if(!element.name().equalsString("value")) {
				continue;
			}

			if(element.value() instanceof AnnotationValue.OfArray array) {
				var path = new ArrayList<String>();
				for(var value : array.values()) {
					if(value instanceof AnnotationValue.OfString str) {
						path.add(str.stringValue());
					}
					else {
						throw new IllegalArgumentException("@ArgonModule value must be a string array");
					}
				}

				return List.copyOf(path);
			}
			else if(element.value() instanceof AnnotationValue.OfString str) {
				return List.of(str.stringValue());
			}

			throw new IllegalArgumentException("@ArgonModule value must be a string array");
		}

		throw new IllegalArgumentException("@ArgonModule must specify a module path");
	}

	private enum ExternKind { FUNCTION, METHOD, STATIC_METHOD }
	private record ExternAnnotation(ExternKind kind, String name) {}

	private static Optional<ExternAnnotation> externAnnotation(MethodModel method) {
		ExternAnnotation result = null;
		for(var annotation : annotations(method)) {
			var kind = switch(annotation.classSymbol().descriptorString()) {
				case EXTERN_FUNCTION_DESCRIPTOR -> ExternKind.FUNCTION;
				case EXTERN_METHOD_DESCRIPTOR -> ExternKind.METHOD;
				case EXTERN_STATIC_METHOD_DESCRIPTOR -> ExternKind.STATIC_METHOD;
				default -> null;
			};
			if(kind == null) continue;
			if(result != null) throw new IllegalArgumentException("Method has multiple extern annotations: "
				+ method.methodName().stringValue());
			result = new ExternAnnotation(kind,
				annotationValue(annotation).orElse(method.methodName().stringValue()));
		}
		return Optional.ofNullable(result);
	}

	private static List<Annotation> annotations(AttributedElement element) {
		var annotations = new ArrayList<Annotation>();
		for(var attribute : element.findAttributes(Attributes.runtimeVisibleAnnotations())) {
			annotations.addAll(attribute.annotations());
		}
		for(var attribute : element.findAttributes(Attributes.runtimeInvisibleAnnotations())) {
			annotations.addAll(attribute.annotations());
		}

		return annotations;
	}

	private static Optional<String> annotationValue(Annotation annotation) {
		for(var element : annotation.elements()) {
			if(!element.name().equalsString("value")) {
				continue;
			}

			if(element.value() instanceof AnnotationValue.OfString str) {
				var value = str.stringValue();
				return value.isEmpty() ? Optional.empty() : Optional.of(value);
			}

			throw new IllegalArgumentException("Extern annotation value must be a string");
		}

		return Optional.empty();
	}

	private static void validateExternMethod(ClassModel classModel, MethodModel method, String externName) {
		var flags = method.flags();
		if(!flags.has(AccessFlag.PUBLIC) || !flags.has(AccessFlag.STATIC)) {
			throw new IllegalArgumentException("Extern function " + externName + " must be a public static method: "
				+ classModel.thisClass().asInternalName() + "." + method.methodName().stringValue()
				+ method.methodType().stringValue());
		}
	}
}
