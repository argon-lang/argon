package dev.argon.options

import dev.argon.util.given

trait OptionDecoder[A] {
  def decodeValue(value: String): OptionDecodeResult.Single[A]
  def decodeAdditionalValue(prev: A, value: String): OptionDecodeResult[A]
}

object OptionDecoder {

  implicit def decoderFactoryStringElem: OptionDecoder[String] = new OptionDecoder[String] {
    override def decodeValue(value: String): OptionDecodeResult.Single[String] = OptionDecodeResult.Result(value)
    override def decodeAdditionalValue(prev: String, value: String): OptionDecodeResult[String] = OptionDecodeResult.MultipleValuesNotSupported
  }

  implicit def decoderFactoryFileListElem: OptionDecoder[FileList] = new OptionDecoder[FileList] {
    override def decodeValue(value: String): OptionDecodeResult.Single[FileList] =
      OptionDecodeResult.Result(new FileList(Seq(value)))

    override def decodeAdditionalValue(prev: FileList, value: String): OptionDecodeResult[FileList] =
      OptionDecodeResult.Result(new FileList(prev.files :+ value))
  }

  implicit def decoderFactorySingleFileElem: OptionDecoder[SingleFile] = new OptionDecoder[SingleFile] {
    override def decodeValue(value: String): OptionDecodeResult.Single[SingleFile] =
      OptionDecodeResult.Result(new SingleFile(value))

    override def decodeAdditionalValue(prev: SingleFile, value: String): OptionDecodeResult[SingleFile] =
      OptionDecodeResult.MultipleValuesNotSupported
  }


  implicit def decoderFactoryOptionElem[A](implicit innerDecoder: OptionDecoder[A]): OptionDecoder[Option[A]] = new OptionDecoder[Option[A]] {
    override def decodeValue(value: String): OptionDecodeResult.Single[Option[A]] =
      innerDecoder.decodeValue(value).map(Some.apply)

    override def decodeAdditionalValue(prev: Option[A], value: String): OptionDecodeResult[Option[A]] =
      prev match {
        case None => decodeValue(value)
        case Some(prevValue) => innerDecoder.decodeAdditionalValue(prevValue, value).map(Some.apply)
      }
  }

  implicit def decoderFactoryListElem[A](implicit innerDecoder: OptionDecoder[A]): OptionDecoder[Seq[A]] = new OptionDecoder[Seq[A]] {
    override def decodeValue(value: String): OptionDecodeResult.Single[Seq[A]] =
      innerDecoder.decodeValue(value).map(List(_))

    override def decodeAdditionalValue(prev: Seq[A], value: String): OptionDecodeResult[Seq[A]] =
      innerDecoder.decodeValue(value).map { decoded => prev :+ decoded }
  }
}
