package dev.argon.libraryexterns;

import dev.argon.externcompiler.ExternGenerator;
import dev.argon.externcompiler.ExternGeneratorSupport;
import java.lang.classfile.ClassBuilder;
import java.lang.classfile.ClassFile;
import java.lang.classfile.CodeBuilder;
import java.lang.classfile.TypeKind;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

public final class IoExternGenerator implements ExternGenerator {
	private static final ClassDesc PATH = cd("argontube2.Argon.IO.Path"), PATH_IMPL = cd("argontube2.Argon.IO.PathImpl");
	private static final ClassDesc INPUT = cd("argontube2.Argon.IO.InputStream"), INPUT_IMPL = cd("argontube2.Argon.IO.InputStreamImpl");
	private static final ClassDesc OUTPUT = cd("argontube2.Argon.IO.OutputStream"), OUTPUT_IMPL = cd("argontube2.Argon.IO.OutputStreamImpl");
	private static final ClassDesc RESOURCE = cd("argontube2.Argon.IO.Resource"), RESOURCE_IMPL = cd("argontube2.Argon.IO.ResourceImpl");
	private static final ClassDesc BIG_INT = cd("java.math.BigInteger"), CLOSEABLE = cd("java.io.Closeable");
	private static final ClassDesc JAVA_INPUT = cd("java.io.InputStream"), JAVA_OUTPUT = cd("java.io.OutputStream");
	private static final ClassDesc TYPE_INFO = cd("dev.argon.runtime.TypeInfo");

	@Override public Collection<byte[]> generate() {
		return List.of(pathExterns(), pathImpl(), inputImpl(), outputImpl(), resourceImpl());
	}

	private static ClassDesc cd(String name) { return ClassDesc.of(name); }

	private static byte[] pathExterns() {
		var desc = MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_String);
		return ExternGeneratorSupport.staticExternMethodClass(cd("argontube2.Argon.IO.PathExterns"),
			ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL, "path_from_string", "fromString", desc, code -> {
				ExternGeneratorSupport.result(code, v -> v.new_(PATH_IMPL).dup().aload(0).invokespecial(PATH_IMPL,
					ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String)));
				code.areturn();
			});
	}

	private static byte[] pathImpl() {
		return ExternGeneratorSupport.CLASS_FILE.build(PATH_IMPL, cb -> cb
			.withFlags(ClassFile.ACC_FINAL | ClassFile.ACC_SUPER).withInterfaceSymbols(PATH)
			.withField("path", ConstantDescs.CD_String, ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL)
			.withMethodBody(ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_String), 0,
				code -> constructor(code, PATH_IMPL, "path", ConstantDescs.CD_String))
			.withMethodBody("display:a:r:bstring:a:e", MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE),
				ClassFile.ACC_PUBLIC, code -> resultReturn(code, v -> v.aload(0).getfield(PATH_IMPL, "path", ConstantDescs.CD_String)))
			.withMethodBody("open_read:a:r:rResource:a:_:r:_:a:rInputStream:a:r:_:a:e:e",
				MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE), ClassFile.ACC_PUBLIC,
				code -> open(code, JAVA_INPUT, INPUT_IMPL, INPUT, "newInputStream"))
			.withMethodBody("open_write:a:r:rResource:a:_:r:_:a:rOutputStream:a:r:_:a:e:e",
				MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE), ClassFile.ACC_PUBLIC,
				code -> open(code, JAVA_OUTPUT, OUTPUT_IMPL, OUTPUT, "newOutputStream")));
	}

	private static void open(CodeBuilder code, ClassDesc javaStream, ClassDesc impl, ClassDesc argonStream, String method) {
		int local = code.allocateLocal(TypeKind.REFERENCE);
		ClassDesc nioPath = cd("java.nio.file.Path"), option = cd("java.nio.file.OpenOption");
		code.aload(0).getfield(PATH_IMPL, "path", ConstantDescs.CD_String).iconst_0().anewarray(ConstantDescs.CD_String)
			.invokestatic(cd("java.nio.file.Paths"), "get", MethodTypeDesc.of(nioPath, ConstantDescs.CD_String,
				ConstantDescs.CD_String.arrayType())).iconst_0().anewarray(option)
			.invokestatic(cd("java.nio.file.Files"), method, MethodTypeDesc.of(javaStream, nioPath, option.arrayType()))
			.astore(local);
		resultReturn(code, v -> v.new_(RESOURCE_IMPL).dup().new_(impl).dup().aload(local)
			.invokespecial(impl, ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void, javaStream))
			.aload(local).ldc(argonStream).invokespecial(RESOURCE_IMPL, ConstantDescs.INIT_NAME,
				MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Object, CLOSEABLE, ConstantDescs.CD_Class)));
	}

	private static byte[] inputImpl() {
		return streamClass(INPUT_IMPL, INPUT, JAVA_INPUT, cb -> cb.withMethodBody(
			"read:a:barray:a:bu8:a:e:e:bint:a:e:bint:a:e:_:r:bint:a:e",
			MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_byte.arrayType(), BIG_INT, BIG_INT),
			ClassFile.ACC_PUBLIC, code -> resultReturn(code, v -> v.aload(0).getfield(INPUT_IMPL, "stream", JAVA_INPUT)
				.aload(1).aload(2).invokevirtual(BIG_INT, "intValueExact", MethodTypeDesc.of(ConstantDescs.CD_int))
				.aload(3).invokevirtual(BIG_INT, "intValueExact", MethodTypeDesc.of(ConstantDescs.CD_int))
				.invokevirtual(JAVA_INPUT, "read", MethodTypeDesc.of(ConstantDescs.CD_int,
					ConstantDescs.CD_byte.arrayType(), ConstantDescs.CD_int, ConstantDescs.CD_int))
				.iconst_0().invokestatic(cd("java.lang.Math"), "max", MethodTypeDesc.of(ConstantDescs.CD_int,
					ConstantDescs.CD_int, ConstantDescs.CD_int)).i2l()
				.invokestatic(BIG_INT, "valueOf", MethodTypeDesc.of(BIG_INT, ConstantDescs.CD_long)))));
	}

	private static byte[] outputImpl() {
		return streamClass(OUTPUT_IMPL, OUTPUT, JAVA_OUTPUT, cb -> cb.withMethodBody(
			"write:a:barray:a:bu8:a:e:e:bint:a:e:bint:a:e:_:r:t:e",
			MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE, ConstantDescs.CD_byte.arrayType(), BIG_INT, BIG_INT),
			ClassFile.ACC_PUBLIC, code -> {
				code.aload(0).getfield(OUTPUT_IMPL, "stream", JAVA_OUTPUT).aload(1).aload(2)
					.invokevirtual(BIG_INT, "intValueExact", MethodTypeDesc.of(ConstantDescs.CD_int)).aload(3)
					.invokevirtual(BIG_INT, "intValueExact", MethodTypeDesc.of(ConstantDescs.CD_int))
					.invokevirtual(JAVA_OUTPUT, "write", MethodTypeDesc.of(ConstantDescs.CD_void,
						ConstantDescs.CD_byte.arrayType(), ConstantDescs.CD_int, ConstantDescs.CD_int));
				unitResult(code);
			}));
	}

	private static byte[] streamClass(ClassDesc name, ClassDesc iface, ClassDesc stream, Consumer<ClassBuilder> methods) {
		return ExternGeneratorSupport.CLASS_FILE.build(name, cb -> {
			cb.withFlags(ClassFile.ACC_FINAL | ClassFile.ACC_SUPER).withInterfaceSymbols(iface)
				.withField("stream", stream, ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL)
				.withMethodBody(ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void, stream), 0,
					code -> constructor(code, name, "stream", stream));
			methods.accept(cb);
		});
	}

	private static byte[] resourceImpl() {
		return ExternGeneratorSupport.CLASS_FILE.build(RESOURCE_IMPL, cb -> cb
			.withFlags(ClassFile.ACC_FINAL | ClassFile.ACC_SUPER).withInterfaceSymbols(RESOURCE)
			.withField("resource", ConstantDescs.CD_Object, ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL)
			.withField("closeable", CLOSEABLE, ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL)
			.withField("type", ConstantDescs.CD_Class, ClassFile.ACC_PRIVATE | ClassFile.ACC_FINAL)
			.withMethodBody(ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void,
				ConstantDescs.CD_Object, CLOSEABLE, ConstantDescs.CD_Class), 0, code -> code.aload(0)
					.invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void)
					.aload(0).aload(1).putfield(RESOURCE_IMPL, "resource", ConstantDescs.CD_Object)
					.aload(0).aload(2).putfield(RESOURCE_IMPL, "closeable", CLOSEABLE)
					.aload(0).aload(3).putfield(RESOURCE_IMPL, "type", ConstantDescs.CD_Class).return_())
			.withMethodBody(":pt0", MethodTypeDesc.of(TYPE_INFO), ClassFile.ACC_PUBLIC, code -> code.new_(TYPE_INFO).dup()
				.aload(0).getfield(RESOURCE_IMPL, "type", ConstantDescs.CD_Class).invokespecial(TYPE_INFO,
					ConstantDescs.INIT_NAME, MethodTypeDesc.of(ConstantDescs.CD_void, ConstantDescs.CD_Class)).areturn())
			.withMethodBody("resource:a:r:_", MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE),
				ClassFile.ACC_PUBLIC, code -> resultReturn(code,
					v -> v.aload(0).getfield(RESOURCE_IMPL, "resource", ConstantDescs.CD_Object)))
			.withMethodBody("close:a:t:e:r:t:e", MethodTypeDesc.of(ExternGeneratorSupport.CD_TRAMPOLINE,
				ExternGeneratorSupport.CD_TUPLE0), ClassFile.ACC_PUBLIC, code -> {
					code.aload(0).getfield(RESOURCE_IMPL, "closeable", CLOSEABLE)
						.invokeinterface(CLOSEABLE, "close", ConstantDescs.MTD_void);
					unitResult(code);
				}));
	}

	private static void constructor(CodeBuilder code, ClassDesc owner, String field, ClassDesc type) {
		code.aload(0).invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void)
			.aload(0).aload(1).putfield(owner, field, type).return_();
	}

	private static void resultReturn(CodeBuilder code, Consumer<CodeBuilder> value) {
		ExternGeneratorSupport.result(code, value);
		code.areturn();
	}

	private static void unitResult(CodeBuilder code) {
		resultReturn(code, v -> v.new_(ExternGeneratorSupport.CD_TUPLE0).dup().invokespecial(
			ExternGeneratorSupport.CD_TUPLE0, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void));
	}
}
