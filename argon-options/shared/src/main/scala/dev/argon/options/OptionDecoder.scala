package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven

trait OptionDecoder[R, E, A] {
  def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, A]
  def defaultValue: Option[A] = None
}

object OptionDecoder {
  final class OptionDecoderDerivation[R, E](using ResourceFactory[R, E]) extends ProductDerivation[[A] =>> OptionDecoder[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionDecoder[R, E, T] =
      new OptionDecoder[R, E, T] {
        override def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, T] =
          value match {
            case Toml.Table(map) =>
              ctx.constructEither { param =>
                map.get(param.label) match {
                  case Some(memberValue) => param.typeclass.decode(memberValue)
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

  inline def derive[R, E, A](using Mirror.Of[A], ResourceFactory[R, E]): OptionDecoder[R, E, A] =
    new OptionDecoderDerivation[R, E].derivedMirror[A]


  given tomlOptionDecoder[R, E, A: TomlCodec]: OptionDecoder[R, E, A] with
    override def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, A] =
      summon[TomlCodec[A]].decode(value)

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue
  end tomlOptionDecoder

  given [R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Option[A]] with
    override def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, Option[A]] =
      summon[OptionDecoder[R, E, A]].decode(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  end given

  given [R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Seq[A]] with
    override def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[R, E, A]].decode)

        case _ => Left("Expected array")
      }
  end given

  given [Res[-R2, +E2] <: Resource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, DirectoryResource[R, E, Res]] with
    override def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, DirectoryResource[R, E, Res]] =
      value match {
        case Toml.String(str) => Right(resourceFactory.directoryResource(str).decode[Res])
        case _ => Left("Expected string")
      }
  end given

  given [Res[_, _], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, Res[R, E]] with
    override def decode(value: Toml)(using resourceFactory: ResourceFactory[R, E]): Either[String, Res[R, E]] =
      value match {
        case Toml.String(str) => Right(summon[BinaryResourceDecoder[Res, R, E]].decode(resourceFactory.binaryResource(str)))
        case _ => Left("Expected string")
      }
  end given

}
