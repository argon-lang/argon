package dev.argon.externcompiler;

import java.lang.classfile.Annotation;
import java.lang.classfile.AnnotationElement;
import java.lang.classfile.AnnotationValue;
import java.lang.classfile.ClassFile;
import java.lang.classfile.CodeBuilder;
import java.lang.classfile.attribute.RuntimeInvisibleAnnotationsAttribute;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.util.function.Consumer;

public final class ExternGeneratorSupport {
	public static final ClassFile CLASS_FILE = ClassFile.of();
	public static final ClassDesc CD_EXTERN_FUNCTION = ClassDesc.of("dev.argon.runtime.ExternFunction");
	public static final ClassDesc CD_EXTERN_METHOD = ClassDesc.of("dev.argon.runtime.ExternMethod");
	public static final ClassDesc CD_EXTERN_STATIC_METHOD = ClassDesc.of("dev.argon.runtime.ExternStaticMethod");
	public static final ClassDesc CD_TRAMPOLINE = ClassDesc.of("dev.argon.runtime.Trampoline");
	public static final ClassDesc CD_TRAMPOLINE_RESULT = ClassDesc.of("dev.argon.runtime.Trampoline$Result");
	public static final ClassDesc CD_TUPLE0 = ClassDesc.of("dev.argon.runtime.Tuple0");

	private ExternGeneratorSupport() {
	}

	public static void result(CodeBuilder code, Consumer<CodeBuilder> value) {
		code.new_(CD_TRAMPOLINE_RESULT).dup();
		value.accept(code);
		code.invokespecial(CD_TRAMPOLINE_RESULT, "<init>",
			MethodTypeDesc.of(java.lang.constant.ConstantDescs.CD_void, java.lang.constant.ConstantDescs.CD_Object));
	}

	public static RuntimeInvisibleAnnotationsAttribute externFunction(String externName) {
		return externAnnotation(CD_EXTERN_FUNCTION, externName);
	}

	public static RuntimeInvisibleAnnotationsAttribute externMethod(String externName) {
		return externAnnotation(CD_EXTERN_METHOD, externName);
	}

	public static RuntimeInvisibleAnnotationsAttribute externStaticMethod(String externName) {
		return externAnnotation(CD_EXTERN_STATIC_METHOD, externName);
	}

	private static RuntimeInvisibleAnnotationsAttribute externAnnotation(ClassDesc annotationClass, String externName) {
		return RuntimeInvisibleAnnotationsAttribute
			.of(Annotation.of(annotationClass, AnnotationElement.of("value", AnnotationValue.of(externName))));
	}

	public static byte[] staticExternClass(ClassDesc className, int classFlags, String externName, String methodName,
		MethodTypeDesc descriptor, Consumer<CodeBuilder> body) {
		return staticExternClass(className, classFlags, externFunction(externName), methodName, descriptor, body);
	}

	public static byte[] staticExternMethodClass(ClassDesc className, int classFlags, String externName,
		String methodName, MethodTypeDesc descriptor, Consumer<CodeBuilder> body) {
		return staticExternClass(className, classFlags, externStaticMethod(externName), methodName, descriptor, body);
	}

	private static byte[] staticExternClass(ClassDesc className, int classFlags,
		RuntimeInvisibleAnnotationsAttribute annotation, String methodName, MethodTypeDesc descriptor,
		Consumer<CodeBuilder> body) {
		return CLASS_FILE.build(className,
			classBuilder -> classBuilder.withFlags(classFlags).withMethod(methodName, descriptor,
				ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
				methodBuilder -> methodBuilder.with(annotation).withCode(body)));
	}
}
