package dev.argon.compiler.options

trait CodecSelector[A, I] {
  def codec[Codec[_]](implicit primitiveCodecs: PrimitiveCodecs[Codec, I]): Codec[A]
}

object CodecSelector {
  object Instances {
    implicit def resourceIndicatorCodecSelector[I]: CodecSelector[I, I] = new CodecSelector[I, I] {
      override def codec[Codec[_]](implicit primitiveCodecs: PrimitiveCodecs[Codec, I]): Codec[I] =
        primitiveCodecs.resourceIndicatorCodec
    }

    implicit def stringCodecSelector[I]: CodecSelector[String, I] = new CodecSelector[String, I] {
      override def codec[Codec[_]](implicit primitiveCodecs: PrimitiveCodecs[Codec, I]): Codec[String] =
        primitiveCodecs.stringCodec
    }

    implicit def fileListCodecSelector[I, A: CodecSelector[*, I]]: CodecSelector[FileList[A], I] = new CodecSelector[FileList[A], I] {
      override def codec[Codec[_]](implicit primitiveCodecs: PrimitiveCodecs[Codec, I]): Codec[FileList[A]] =
        primitiveCodecs.fileListCodec(implicitly[CodecSelector[A, I]].codec)
    }

    implicit def singleFileCodecSelector[I, A: CodecSelector[*, I]]: CodecSelector[SingleFile[A], I] = new CodecSelector[SingleFile[A], I] {
      override def codec[Codec[_]](implicit primitiveCodecs: PrimitiveCodecs[Codec, I]): Codec[SingleFile[A]] =
        primitiveCodecs.singleFileCodec(implicitly[CodecSelector[A, I]].codec)
    }

    implicit def optionCodecSelector[I, A: CodecSelector[*, I]]: CodecSelector[Option[A], I] = new CodecSelector[Option[A], I] {
      override def codec[Codec[_]](implicit primitiveCodecs: PrimitiveCodecs[Codec, I]): Codec[Option[A]] =
        primitiveCodecs.optionCodec(implicitly[CodecSelector[A, I]].codec)
    }
  }
}

