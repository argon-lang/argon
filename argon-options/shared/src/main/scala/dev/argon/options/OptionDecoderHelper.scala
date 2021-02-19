package dev.argon.options

import shapeless.{Id => _, _}

object OptionDecoderHelper {

  trait DecoderFactory[L] {
    def create: L
  }

  implicit def decoderFactoryNil: DecoderFactory[HNil] = new DecoderFactory[HNil] {
    override def create: HNil = HNil
  }

  implicit def decoderFactoryCons[H, T <: HList](implicit headFactory: DecoderFactory[H], tailFactory: DecoderFactory[T]): DecoderFactory[H :: T] = new DecoderFactory[H :: T] {
    override def create: H :: T = headFactory.create :: tailFactory.create
  }


  implicit def decoderFactoryStringElem: DecoderFactory[OptionDecoder[String]] = new DecoderFactory[OptionDecoder[String]] {
    override def create: OptionDecoder[String] = new OptionDecoder[String] {
      override def decodeValue(value: String): OptionDecodeResult.Single[String] = OptionDecodeResult.Result(value)
      override def decodeAdditionalValue(prev: String, value: String): OptionDecodeResult[String] = OptionDecodeResult.MultipleValuesNotSupported
    }
  }

  implicit def decoderFactoryFileListElem: DecoderFactory[OptionDecoder[FileList]] = new DecoderFactory[OptionDecoder[FileList]] {
    override def create: OptionDecoder[FileList] = new OptionDecoder[FileList] {
      override def decodeValue(value: String): OptionDecodeResult.Single[FileList] =
        OptionDecodeResult.Result(new FileList(List(value)))

      override def decodeAdditionalValue(prev: FileList, value: String): OptionDecodeResult[FileList] =
        OptionDecodeResult.Result(new FileList(prev.files :+ value))
    }
  }

  implicit def decoderFactorySingleFileElem: DecoderFactory[OptionDecoder[SingleFile]] = new DecoderFactory[OptionDecoder[SingleFile]] {
    override def create: OptionDecoder[SingleFile] = new OptionDecoder[SingleFile] {
      override def decodeValue(value: String): OptionDecodeResult.Single[SingleFile] =
        OptionDecodeResult.Result(new SingleFile(value))

      override def decodeAdditionalValue(prev: SingleFile, value: String): OptionDecodeResult[SingleFile] =
        OptionDecodeResult.MultipleValuesNotSupported
    }
  }


  implicit def decoderFactoryOptionElem[A](implicit innerDecoderFactory: DecoderFactory[OptionDecoder[A]]): DecoderFactory[OptionDecoder[Option[A]]] = new DecoderFactory[OptionDecoder[Option[A]]] {
    override def create: OptionDecoder[Option[A]] = new OptionDecoder[Option[A]] {
      private val innerDecoder = innerDecoderFactory.create

      override def decodeValue(value: String): OptionDecodeResult.Single[Option[A]] =
        innerDecoder.decodeValue(value).map(Some.apply)

      override def decodeAdditionalValue(prev: Option[A], value: String): OptionDecodeResult[Option[A]] =
        prev match {
          case None => decodeValue(value)
          case Some(prevValue) => innerDecoder.decodeAdditionalValue(prevValue, value).map(Some.apply)
        }
    }
  }

  implicit def decoderFactoryListElem[A](implicit innerDecoderFactory: DecoderFactory[OptionDecoder[A]]): DecoderFactory[OptionDecoder[List[A]]] = new DecoderFactory[OptionDecoder[List[A]]] {
    override def create: OptionDecoder[List[A]] = new OptionDecoder[List[A]] {
      private val innerDecoder = innerDecoderFactory.create

      override def decodeValue(value: String): OptionDecodeResult.Single[List[A]] =
        innerDecoder.decodeValue(value).map(List(_))

      override def decodeAdditionalValue(prev: List[A], value: String): OptionDecodeResult[List[A]] =
        innerDecoder.decodeValue(value).map { decoded => prev :+ decoded }
    }
  }


  def createDecoders[L](implicit decoderFactory: DecoderFactory[L]): L = decoderFactory.create
}
