package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import java.io.IOException
import zio.*

trait OptionDecoder[E, A] {
  def decode(value: Toml): ZIO[ResourceFactory, String, A]
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



  final class OptionDecoderDerivation[E] extends ProductDerivation[[A] =>> OptionDecoder[E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionDecoder[E, T] =
      new OptionDecoder[E, T] {
        override def decode(value: Toml): ZIO[ResourceFactory, String, T] =
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
              }

            case _ =>
              ZIO.fail("Expected table")
          }
      }

  }

  inline def derive[E, A](using Mirror.Of[A]): OptionDecoder[E, A] =
    new OptionDecoderDerivation[E].derivedMirror[A]


  given tomlOptionDecoder[E, A: TomlCodec]: OptionDecoder[E, A] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, A] =
      ZIO.fromEither(summon[TomlCodec[A]].decode(value))

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue
  end tomlOptionDecoder

  given [E, A](using OptionDecoder[E, A], NotGiven[TomlCodec[A]]): OptionDecoder[E, Option[A]] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Option[A]] =
      summon[OptionDecoder[E, A]].decode(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  end given

  given [E, A](using OptionDecoder[E, A], NotGiven[TomlCodec[A]]): OptionDecoder[E, Seq[A]] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[E, A]].decode)

        case _ => ZIO.fail("Expected array")
      }
  end given

  given [Res[-R2, +E2] <: Resource[R2, E2], E >: IOException](using BinaryResourceDecoder[Res, Any, E]): OptionDecoder[E, DirectoryResource[Any, E, Res]] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, DirectoryResource[Any, E, Res]] =
      value match {
        case Toml.String(str) =>
          ZIO.serviceWith[ResourceFactory] { rf =>
            (rf.directoryResource(str) : DirectoryResource[Any, E, BinaryResource]).decode[Res]
          }

        case _ => ZIO.fail("Expected string")
      }
  end given

  given [Res[_, _], E >: IOException](using BinaryResourceDecoder[Res, Any, E]): OptionDecoder[E, Res[Any, E]] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Res[Any, E]] =
      value match {
        case Toml.String(str) =>
          ZIO.serviceWith[ResourceFactory] { rf =>
            summon[BinaryResourceDecoder[Res, Any, E]].decode(rf.binaryResource(str))
          }

        case _ => ZIO.fail("Expected string")
      }
  end given

}
