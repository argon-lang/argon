package dev.argon.jvmbackendmetadata;

import dev.argon.esexpr.*;

import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;

public record Classfile(ClassModel model) {
	private static final ClassFile CLASS_FILE = ClassFile.of();
	private static final ESExprCodec<byte[]> BYTE_ARRAY_CODEC = ESExprCodec.byteArrayCodec();

	private static final ESExprCodec<Classfile> CODEC = new ESExprCodec<>() {
		@Override
		public ESExprTagSet tags() {
			return BYTE_ARRAY_CODEC.tags();
		}

		@Override
		public ESExpr encode(Classfile value) {
			return BYTE_ARRAY_CODEC.encode(value.toByteArray());
		}

		@Override
		public Classfile decode(ESExpr expr, FailurePath path) throws DecodeException {
			var bytes = BYTE_ARRAY_CODEC.decode(expr, path);
			try {
				return Classfile.parse(bytes);
			}
			catch(IllegalArgumentException ex) {
				throw new DecodeException("Invalid classfile: " + ex.getMessage(), path);
			}
		}

		@Override
		public boolean isEncodedEqual(Classfile x, Classfile y) {
			return BYTE_ARRAY_CODEC.isEncodedEqual(x.toByteArray(), y.toByteArray());
		}
	};

	public static Classfile parse(byte[] bytes) {
		return new Classfile(CLASS_FILE.parse(bytes));
	}

	public byte[] toByteArray() {
		return CLASS_FILE.transformClass(model, ClassTransform.ACCEPT_ALL);
	}

	@TypeClassInstance
	@ESExprCodecTags(scalar = {ESExprTag.Scalar.ARRAY8})
	public static ESExprCodec<Classfile> codec() {
		return CODEC;
	}
}
