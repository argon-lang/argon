package dev.argon.libraryexterns;

import dev.argon.externcompiler.ExternGenerator;
import dev.argon.externcompiler.ExternGeneratorSupport;
import java.lang.classfile.ClassFile;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.util.Collection;
import java.util.List;

public final class TestExternGenerator implements ExternGenerator {
	@Override
	public Collection<byte[]> generate() {
		var className = ClassDesc.of("argontube2.Argon.TestExterns.TestExterns");
		var stringResult = MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ClassDesc.of("java.lang.String"));
		var bytes = ExternGeneratorSupport.CLASS_FILE.build(className, cb -> cb
			.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL)
			.withMethod("functionResult", MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE),
				ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, mb -> mb
					.with(ExternGeneratorSupport.externFunction("test_extern_function"))
					.withCode(code -> code.ldc("function result").invokestatic(
						ExternGeneratorSupport.CD_EXTERN_SUPPORT, "stringResult", stringResult).areturn()))
			.withMethod("methodResult", MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE,
				ExternGeneratorSupport.CD_TUPLE0),
				ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, mb -> mb
					.with(ExternGeneratorSupport.externMethod("test_extern_method"))
					.withCode(code -> code.ldc("method result").invokestatic(
						ExternGeneratorSupport.CD_EXTERN_SUPPORT, "stringResult", stringResult).areturn()))
			.withMethod("staticMethodResult", MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE),
				ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC, mb -> mb
					.with(ExternGeneratorSupport.externStaticMethod("test_extern_static_method"))
					.withCode(code -> code.ldc("static method result").invokestatic(
						ExternGeneratorSupport.CD_EXTERN_SUPPORT, "stringResult", stringResult).areturn())));
		return List.of(bytes);
	}
}
