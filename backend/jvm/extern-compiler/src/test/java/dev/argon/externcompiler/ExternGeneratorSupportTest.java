package dev.argon.externcompiler;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.classfile.Attributes;
import java.lang.classfile.ClassFile;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import org.junit.jupiter.api.Test;

final class ExternGeneratorSupportTest {
	@Test
	void buildsAnnotatedStaticExternMethod() {
		MethodTypeDesc descriptor = MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_String);
		byte[] bytes = ExternGeneratorSupport.staticExternClass(ClassDesc.of("test.GeneratedExtern"),
			ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL, "generated_extern", "invoke", descriptor,
			code -> code.aconst_null().areturn());

		var model = ClassFile.of().parse(bytes);
		assertEquals("test/GeneratedExtern", model.thisClass().asInternalName());
		var method = model.methods().getFirst();
		assertEquals("invoke", method.methodName().stringValue());
		assertTrue(method.flags().has(java.lang.reflect.AccessFlag.PUBLIC));
		assertTrue(method.flags().has(java.lang.reflect.AccessFlag.STATIC));
		var annotations = method.findAttribute(Attributes.runtimeInvisibleAnnotations()).orElseThrow();
		assertEquals(ExternGeneratorSupport.CD_EXTERN_FUNCTION, annotations.annotations().getFirst().classSymbol());
	}
}
