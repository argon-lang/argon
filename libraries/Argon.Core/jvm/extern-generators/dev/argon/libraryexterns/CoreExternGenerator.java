package dev.argon.libraryexterns;

import dev.argon.externcompiler.ExternGenerator;
import dev.argon.externcompiler.ExternGeneratorSupport;
import java.lang.classfile.ClassFile;
import java.lang.classfile.CodeBuilder;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

public final class CoreExternGenerator implements ExternGenerator {
	@Override
	public Collection<byte[]> generate() {
		var classes = new ArrayList<byte[]>();
		primitive(classes, "I8", "i8_to_s", "i8ToS", ConstantDescs.CD_byte, code -> code.iload(0));
		primitive(classes, "I16", "i16_to_s", "i16ToS", ConstantDescs.CD_short, code -> code.iload(0));
		primitive(classes, "I32", "i32_to_s", "i32ToS", ConstantDescs.CD_int, code -> code.iload(0));
		primitive(classes, "I64", "i64_to_s", "i64ToS", ConstantDescs.CD_long, code -> code.lload(0));
		primitive(classes, "U8", "u8_to_s", "u8ToS", ConstantDescs.CD_byte, code -> code.iload(0));
		primitive(classes, "U16", "u16_to_s", "u16ToS", ConstantDescs.CD_short, code -> code.iload(0));
		primitive(classes, "U32", "u32_to_s", "u32ToS", ConstantDescs.CD_int, code -> code.iload(0));
		primitive(classes, "U64", "u64_to_s", "u64ToS", ConstantDescs.CD_long, code -> code.lload(0));
		classes.add(singleMethod(
			"argontube2.Argon.Core.Int.IntExterns", "int_to_s", "intToS", "intToS",
			MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ClassDesc.of("java.math.BigInteger"), ExternGeneratorSupport.CD_TUPLE0),
			code -> code.aload(0).aload(1)
		));
		classes.add(singleMethod(
			"argontube2.Argon.Core.Puts.PutsExterns", "puts", "puts", "puts",
			MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_String), code -> code.aload(0)
		));
		classes.add(stringPredicates());
		return List.copyOf(classes);
	}

	private static byte[] stringPredicates() {
		ClassDesc className = ClassDesc.of("argontube2.Argon.Core.String.StringExterns");
		MethodTypeDesc descriptor = MethodTypeDesc.of(
			ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_String, ConstantDescs.CD_String
		);
		return ExternGeneratorSupport.CLASS_FILE.build(className, classBuilder -> {
			stringPredicate(classBuilder, "string_starts_with", "stringStartsWith", descriptor);
			stringPredicate(classBuilder, "string_ends_with", "stringEndsWith", descriptor);
			stringPredicate(classBuilder, "string_contains", "stringContains", descriptor);
		});
	}

	private static void stringPredicate(
		java.lang.classfile.ClassBuilder classBuilder,
		String externName,
		String methodName,
		MethodTypeDesc descriptor
	) {
		classBuilder.withFlags(ClassFile.ACC_FINAL).withMethod(
			methodName, descriptor, ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
			methodBuilder -> methodBuilder.with(ExternGeneratorSupport.externFunction(externName)).withCode(
				code -> code.aload(0).aload(1)
					.invokestatic(ExternGeneratorSupport.CD_EXTERN_SUPPORT, methodName, descriptor).areturn()
			)
		);
	}

	private static void primitive(
		List<byte[]> classes,
		String module,
		String externName,
		String methodName,
		ClassDesc valueType,
		Consumer<CodeBuilder> loadValue
	) {
		MethodTypeDesc descriptor = MethodTypeDesc.of(
			ExternGeneratorSupport.CD_TRAMPOLINE, valueType, ExternGeneratorSupport.CD_TUPLE0
		);
		classes.add(singleMethod(
			"argontube2.Argon.Core." + module + "." + module + "Externs",
			externName, methodName, methodName, descriptor,
			code -> { loadValue.accept(code); code.aload(valueType.equals(ConstantDescs.CD_long) ? 2 : 1); }
		));
	}

	private static byte[] singleMethod(
		String className,
		String externName,
		String methodName,
		String supportMethod,
		MethodTypeDesc descriptor,
		Consumer<CodeBuilder> loadArguments
	) {
		return ExternGeneratorSupport.staticExternClass(
			ClassDesc.of(className), ClassFile.ACC_FINAL, externName, methodName, descriptor,
			code -> {
				loadArguments.accept(code);
				code.invokestatic(ExternGeneratorSupport.CD_EXTERN_SUPPORT, supportMethod, descriptor).areturn();
			}
		);
	}
}
