package dev.argon.options

import magnolia1.*
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, DirectoryResource, ResourceDecodeException}
import dev.argon.options.OptionDecoder.ResourceFactory
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven

trait OptionDecoder[A, E] {
  def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, A]
  def defaultValue: Option[A] = None
}

object OptionDecoder {
  trait ResourceFactory[E] {
    def directoryResource(name: String): DirectoryResource[E]
    def binaryResource(name: String): BinaryResource[E]
  }

  final class OptionDecoderDerivation[E](using ResourceFactory[E]) extends ProductDerivation[[A] =>> OptionDecoder[A, E]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionDecoder[T, E] =
      new OptionDecoder[T, E] {
        override def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, T] =
          value match {
            case Toml.Table(map) =>
              ctx.constructEither { param =>
                map.get(param.label) match {
                  case Some(memberValue) => param.typeclass.decode(resourceFactory)(memberValue)
                  case None =>
                    param.typeclass.defaultValue match {
                      case Some(memberValue) => Right(memberValue)
                      case None => Left(s"Missing key in table: ${param.label}, map: ${map}")
                    }

                }
              }.left.map { _.mkString("\n") }

            case _ =>
              Left("Expected table")
          }
      }

  }

  inline def derive[A, E](using Mirror.Of[A], ResourceFactory[E]): OptionDecoder[A, E] =
    new OptionDecoderDerivation[E].derivedMirror[A]


  given tomlOptionDecoder[A: TomlCodec, E]: OptionDecoder[A, E] with
    override def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, A] =
      summon[TomlCodec[A]].decode(value)

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue
  end tomlOptionDecoder

  given [A, E](using OptionDecoder[A, E], NotGiven[TomlCodec[A]]): OptionDecoder[Option[A], E] with
    override def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, Option[A]] =
      summon[OptionDecoder[A, E]].decode(resourceFactory)(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  end given

  given [A, E](using OptionDecoder[A, E], NotGiven[TomlCodec[A]]): OptionDecoder[Seq[A], E] with
    override def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[A, E]].decode(resourceFactory))

        case _ => Left("Expected array")
      }
  end given

  given [E]: OptionDecoder[DirectoryResource[E], E] with
    override def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, DirectoryResource[E]] =
      value match {
        case Toml.String(str) => Right(resourceFactory.directoryResource(str))
        case _ => Left("Expected string")
      }
  end given

  given [Res[+_]: BinaryResourceDecoder, E >: ResourceDecodeException]: OptionDecoder[Res[E], E] with
    override def decode(resourceFactory: ResourceFactory[E])(value: Toml): Either[String, Res[E]] =
      value match {
        case Toml.String(str) => Right(summon[BinaryResourceDecoder[Res]].decode(resourceFactory.binaryResource(str)))
        case _ => Left("Expected string")
      }
  end given

}
