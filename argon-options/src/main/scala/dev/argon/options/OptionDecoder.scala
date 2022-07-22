package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import zio.*

trait OptionDecoder[R, E, A] {
  def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, A]
  def defaultValue: Option[A] = None
}

object OptionDecoder {

  private given [R, E]: Monadic[[A] =>> ZIO[R, E, A]] with
    override def point[A](value: A): ZIO[R, E, A] = ZIO.succeed(value)

    override def map[A, B](from: ZIO[R, E, A])(fn: A => B): ZIO[R, E, B] =
      from.map(fn)

    override def flatMap[A, B](from: ZIO[R, E, A])(fn: A => ZIO[R, E, B]): ZIO[R, E, B] =
      from.flatMap(fn)
  end given



  final class OptionDecoderDerivation[R, E] extends ProductDerivation[[A] =>> OptionDecoder[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionDecoder[R, E, T] =
      new OptionDecoder[R, E, T] {
        override def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, T] =
          value match {
            case Toml.Table(map) =>
              ctx.constructMonadic { param =>
                map.get(param.label) match {
                  case Some(memberValue) => param.typeclass.decode(memberValue)
                  case None =>
                    param.typeclass.defaultValue match {
                      case Some(memberValue) => ZIO.succeed(memberValue)
                      case None => ZIO.fail(s"Missing key in table: ${param.label}, map: ${map}")
                    }

                }
              }.mapError { _.mkString("\n") }

            case _ =>
              ZIO.fail("Expected table")
          }
      }

  }

  inline def derive[R, E, A](using Mirror.Of[A]): OptionDecoder[R, E, A] =
    new OptionDecoderDerivation[R, E].derivedMirror[A]


  given tomlOptionDecoder[R, E, A: TomlCodec]: OptionDecoder[R, E, A] with
    override def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, A] =
      ZIO.fromEither(summon[TomlCodec[A]].decode(value))

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue
  end tomlOptionDecoder

  given [R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Option[A]] with
    override def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, Option[A]] =
      summon[OptionDecoder[R, E, A]].decode(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  end given

  given [R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Seq[A]] with
    override def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[R, E, A]].decode)

        case _ => ZIO.fail("Expected array")
      }
  end given

  given [Res[-R2, +E2] <: Resource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, DirectoryResource[R, E, Res]] with
    override def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, DirectoryResource[R, E, Res]] =
      value match {
        case Toml.String(str) =>
          ZIO.serviceWith[RF] { _.directoryResource(str).decode[Res] }

        case _ => ZIO.fail("Expected string")
      }
  end given

  given [Res[_, _], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, Res[R, E]] with
    override def decode[RF <: ResourceFactory[R, E]: Tag](value: Toml): ZIO[RF, String, Res[R, E]] =
      value match {
        case Toml.String(str) =>
          ZIO.serviceWith[RF] { rf =>
            summon[BinaryResourceDecoder[Res, R, E]].decode(rf.binaryResource(str))
          }

        case _ => ZIO.fail("Expected string")
      }
  end given

}
