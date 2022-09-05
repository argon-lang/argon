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
  // Derivation
  private[options] open class DerivationImplDecoder[E >: IOException, T, TDec[A] <: OptionDecoder[E, A]](ctx: CaseClass[TDec, T]) extends OptionDecoder[E, T] {
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


  final class OptionDecoderDerivation[E >: IOException] extends ProductDerivation[[A] =>> OptionDecoder[E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionDecoder[E, T] =
      DerivationImplDecoder(ctx)
  }


  inline def derive[E >: IOException, A](using Mirror.Of[A]): OptionDecoder[E, A] =
    new OptionDecoderDerivation[E].derivedMirror[A]

  // Toml
  given [E >: IOException, A](using TomlCodec[A]): OptionDecoder[E, A] =
    summon[OptionCodec[Any, E, A]]

  // Option
  private[options] open class OptionOptionDecoderBase[E >: IOException, A](using OptionDecoder[E, A]) extends OptionDecoder[E, Option[A]] {
    override def decode(value: Toml): ZIO[ResourceFactory, String, Option[A]] =
      summon[OptionDecoder[E, A]].decode(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  }


  given[E >: IOException, A](using OptionDecoder[E, A], NotGiven[TomlCodec[A]]): OptionDecoder[E, Option[A]] =
    OptionOptionDecoderBase()


  // Seq
  private[options] open class SeqOptionDecoderBase[E >: IOException, A](using OptionDecoder[E, A]) extends OptionDecoder[E, Seq[A]] {
    override def decode(value: Toml): ZIO[ResourceFactory, String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[E, A]].decode)

        case _ => ZIO.fail("Expected array")
      }
  }


  given[E >: IOException, A](using OptionDecoder[E, A], NotGiven[TomlCodec[A]]): OptionDecoder[E, Seq[A]] =
    SeqOptionDecoderBase()


  // DirectoryResource
  private[options] open class DirectoryResourceOptionDecoderBase[Res[-R2, +E2] <: Resource[R2, E2], R, E >: IOException](using BinaryResourceDecoder[Res, R, E]) extends OptionDecoder[E, DirectoryResource[R, E, Res]] {
    override def decode(value: Toml): ZIO[ResourceFactory, String, DirectoryResource[R, E, Res]] =
      value match {
        case Toml.String(str) =>
          ZIO.serviceWith[ResourceFactory] { rf =>
            (rf.directoryResource(str): DirectoryResource[R, E, BinaryResource]).decode[Res]
          }

        case _ => ZIO.fail("Expected string")
      }
  }

  given directoryResourceDecoder[Res[-R2, +E2] <: Resource[R2, E2], R, E >: IOException](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[E, DirectoryResource[R, E, Res]] =
    DirectoryResourceOptionDecoderBase()


  // BinaryResource
  private[options] open class BinaryResourceOptionDecoderBase[Res[_, _], R, E >: IOException](using BinaryResourceDecoder[Res, R, E]) extends OptionDecoder[E, Res[R, E]] {
    override def decode(value: Toml): ZIO[ResourceFactory, String, Res[R, E]] =
      value match {
        case Toml.String(str) =>
          ZIO.serviceWith[ResourceFactory] { rf =>
            summon[BinaryResourceDecoder[Res, R, E]].decode(rf.binaryResource(str))
          }

        case _ => ZIO.fail("Expected string")
      }
  }

  given [Res[R2, E2], R, E >: IOException](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[E, Res[R, E]] =
    BinaryResourceOptionDecoderBase()

}


