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
		primitive(classes, "I8", "i8_to_s", "i8ToS", ConstantDescs.CD_byte, ConstantDescs.CD_Byte,
			"toString", code -> code.iload(0));
		primitive(classes, "I16", "i16_to_s", "i16ToS", ConstantDescs.CD_short, ConstantDescs.CD_Short,
			"toString", code -> code.iload(0));
		primitive(classes, "I32", "i32_to_s", "i32ToS", ConstantDescs.CD_int, ConstantDescs.CD_Integer,
			"toString", code -> code.iload(0));
		primitive(classes, "I64", "i64_to_s", "i64ToS", ConstantDescs.CD_long, ConstantDescs.CD_Long,
			"toString", code -> code.lload(0));
		primitive(classes, "U8", "u8_to_s", "u8ToS", ConstantDescs.CD_byte, ConstantDescs.CD_Integer,
			"toString", code -> code.iload(0).invokestatic(ConstantDescs.CD_Byte, "toUnsignedInt",
				MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_byte)));
		primitive(classes, "U16", "u16_to_s", "u16ToS", ConstantDescs.CD_short, ConstantDescs.CD_Integer,
			"toString", code -> code.iload(0).invokestatic(ConstantDescs.CD_Short, "toUnsignedInt",
				MethodTypeDesc.of(ConstantDescs.CD_int, ConstantDescs.CD_short)));
		primitive(classes, "U32", "u32_to_s", "u32ToS", ConstantDescs.CD_int, ConstantDescs.CD_Integer,
			"toUnsignedString", code -> code.iload(0));
		primitive(classes, "U64", "u64_to_s", "u64ToS", ConstantDescs.CD_long, ConstantDescs.CD_Long,
			"toUnsignedString", code -> code.lload(0));
		classes.add(singleMethod(
			"argontube2.Argon.Core.Int.IntExterns", "int_to_s", "intToS",
			MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ClassDesc.of("java.math.BigInteger"), ExternGeneratorSupport.CD_TUPLE0),
			code -> code.aload(0).invokevirtual(ClassDesc.of("java.math.BigInteger"), "toString",
				MethodTypeDesc.of(ConstantDescs.CD_String))
		));
		classes.add(singleMethod(
			"argontube2.Argon.Core.Puts.PutsExterns", "puts", "puts",
			MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_String), code -> code
				.getstatic(ClassDesc.of("java.lang.System"), "out", ClassDesc.of("java.io.PrintStream"))
				.aload(0).invokevirtual(ClassDesc.of("java.io.PrintStream"), "println",
					MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String))
				.new_(ExternGeneratorSupport.CD_TUPLE0).dup().invokespecial(ExternGeneratorSupport.CD_TUPLE0,
					"<init>", MethodTypeDesc.of(ConstantDescs.CD_void))
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
		String javaMethodName = switch(methodName) {
			case "stringStartsWith" -> "startsWith";
			case "stringEndsWith" -> "endsWith";
			case "stringContains" -> "contains";
			default -> throw new IllegalArgumentException(methodName);
		};
		ClassDesc argumentType = methodName.equals("stringContains")
			? ClassDesc.of("java.lang.CharSequence") : ConstantDescs.CD_String;
		classBuilder.withFlags(ClassFile.ACC_FINAL).withMethod(
			methodName, descriptor, ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
			methodBuilder -> methodBuilder.with(ExternGeneratorSupport.externFunction(externName)).withCode(code -> {
				ExternGeneratorSupport.result(code, value -> value.aload(0).aload(1)
					.invokevirtual(ConstantDescs.CD_String, javaMethodName,
						MethodTypeDesc.of(ConstantDescs.CD_boolean, argumentType))
					.invokestatic(ConstantDescs.CD_Boolean, "valueOf",
						MethodTypeDesc.of(ConstantDescs.CD_Boolean, ConstantDescs.CD_boolean)));
				code.areturn();
			})
		);
	}

	private static void primitive(
		List<byte[]> classes,
		String module,
		String externName,
		String methodName,
		ClassDesc valueType,
		ClassDesc conversionOwner,
		String conversionMethod,
		Consumer<CodeBuilder> loadValue
	) {
		MethodTypeDesc descriptor = MethodTypeDesc.of(
			ExternGeneratorSupport.CD_TRAMPOLINE, valueType, ExternGeneratorSupport.CD_TUPLE0
		);
		classes.add(singleMethod(
			"argontube2.Argon.Core." + module + "." + module + "Externs", externName, methodName, descriptor,
			code -> {
				loadValue.accept(code);
				code.invokestatic(conversionOwner, conversionMethod,
					MethodTypeDesc.of(ConstantDescs.CD_String,
						conversionOwner.equals(ConstantDescs.CD_Integer) ? ConstantDescs.CD_int : valueType));
			}
		));
	}

	private static byte[] singleMethod(
		String className,
		String externName,
		String methodName,
		MethodTypeDesc descriptor,
		Consumer<CodeBuilder> value
	) {
		return ExternGeneratorSupport.staticExternClass(
			ClassDesc.of(className), ClassFile.ACC_FINAL, externName, methodName, descriptor,
			code -> { ExternGeneratorSupport.result(code, value); code.areturn(); }
		);
	}
}
