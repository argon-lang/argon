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
	public static final ClassDesc CD_EXTERN_SUPPORT = ClassDesc.of("dev.argon.runtime.ExternSupport");
	public static final ClassDesc CD_TRAMPOLINE = ClassDesc.of("dev.argon.runtime.Trampoline");
	public static final ClassDesc CD_TUPLE0 = ClassDesc.of("dev.argon.runtime.Tuple0");

	private ExternGeneratorSupport() {
	}

	public static RuntimeInvisibleAnnotationsAttribute externFunction(String externName) {
		return RuntimeInvisibleAnnotationsAttribute
			.of(Annotation.of(CD_EXTERN_FUNCTION, AnnotationElement.of("value", AnnotationValue.of(externName))));
	}

	public static byte[] staticExternClass(ClassDesc className, int classFlags, String externName, String methodName,
		MethodTypeDesc descriptor, Consumer<CodeBuilder> body) {
		return CLASS_FILE.build(className,
			classBuilder -> classBuilder.withFlags(classFlags).withMethod(methodName, descriptor,
				ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
				methodBuilder -> methodBuilder.with(externFunction(externName)).withCode(body)));
	}
}
