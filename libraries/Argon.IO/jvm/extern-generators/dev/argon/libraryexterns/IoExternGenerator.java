package dev.argon.libraryexterns;

import dev.argon.externcompiler.ExternGenerator;
import dev.argon.externcompiler.ExternGeneratorSupport;
import java.lang.classfile.ClassFile;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.util.Collection;
import java.util.List;

public final class IoExternGenerator implements ExternGenerator {
	@Override
	public Collection<byte[]> generate() {
		MethodTypeDesc descriptor = MethodTypeDesc.of(
			ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_String
		);
		byte[] pathExterns = ExternGeneratorSupport.staticExternMethodClass(
			ClassDesc.of("argontube2.Argon.IO.PathExterns"),
			ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL,
			"path_from_string",
			"fromString",
			descriptor,
			code -> code
				.aload(0)
				.invokestatic(ExternGeneratorSupport.CD_EXTERN_SUPPORT, "pathFromString", descriptor)
				.areturn()
		);
		return List.of(pathExterns);
	}
}
