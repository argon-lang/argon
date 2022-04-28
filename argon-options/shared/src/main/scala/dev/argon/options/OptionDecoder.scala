package dev.argon.options

import dev.argon.util.given

trait OptionDecoder[A] {
  def decodeValue(value: String): Either[String => OptionsError.CouldNotDecode, A]
  def decodeAdditionalValue(prev: A, value: String): Either[String => OptionsError.ParseError, A]
  def defaultValue: Option[A]
}

object OptionDecoder {

  implicit def decoderFactoryStringElem: OptionDecoder[String] =
    new OptionDecoder[String] {
      override def decodeValue(value: String): Either[String => OptionsError.CouldNotDecode, String] = Right(value)

      override def decodeAdditionalValue(prev: String, value: String): Either[String => OptionsError.ParseError, String] =
        Left(OptionsError.MultipleValuesNotSupported.apply)

      override def defaultValue: Option[String] = None
    }

  implicit def decoderFactoryFileListElem: OptionDecoder[FileList] =
    new OptionDecoder[FileList] {

      override def decodeValue(value: String): Either[String => OptionsError.CouldNotDecode, FileList] =
        Right(new FileList(Seq(value)))

      override def decodeAdditionalValue(prev: FileList, value: String): Either[String => OptionsError.ParseError, FileList] =
        Right(new FileList(prev.files :+ value))

      override def defaultValue: Option[FileList] = Some(FileList(Seq()))
    }

  implicit def decoderFactorySingleFileElem: OptionDecoder[SingleFile] =
    new OptionDecoder[SingleFile] {

      override def decodeValue(value: String): Either[String => OptionsError.CouldNotDecode, SingleFile] =
        Right(new SingleFile(value))

      override def decodeAdditionalValue(prev: SingleFile, value: String): Either[String => OptionsError.ParseError, SingleFile] =
        Left(OptionsError.MultipleValuesNotSupported.apply)

      override def defaultValue: Option[SingleFile] = None

    }

  implicit def decoderFactoryOptionElem[A](implicit innerDecoder: OptionDecoder[A]): OptionDecoder[Option[A]] =
    new OptionDecoder[Option[A]] {

      override def decodeValue(value: String): Either[String => OptionsError.CouldNotDecode, Option[A]] =
        innerDecoder.decodeValue(value).map(Some.apply)

      override def decodeAdditionalValue(prev: Option[A], value: String): Either[String => OptionsError.ParseError, Option[A]] =
        prev match {
          case None => decodeValue(value)
          case Some(prevValue) => innerDecoder.decodeAdditionalValue(prevValue, value).map(Some.apply)
        }

      override def defaultValue: Option[Option[A]] = Some(None)
    }

  implicit def decoderFactoryListElem[A](implicit innerDecoder: OptionDecoder[A]): OptionDecoder[Seq[A]] =
    new OptionDecoder[Seq[A]] {

      override def decodeValue(value: String): Either[String => OptionsError.CouldNotDecode, Seq[A]] =
        innerDecoder.decodeValue(value).map(List(_))

      override def decodeAdditionalValue(prev: Seq[A], value: String): Either[String => OptionsError.ParseError, Seq[A]] =
        innerDecoder.decodeValue(value).map { decoded => prev :+ decoded }

      override def defaultValue: Option[Seq[A]] = Some(Seq.empty)
    }

}
