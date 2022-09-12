package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import zio.*

trait OptionDecoder[R, E, A] {
  def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, A]
  def defaultValue: Option[A] = None
}

object OptionDecoder {
  // Derivation
  private[options] open class DerivationImplDecoder[R, E, T, TDec[A] <: OptionDecoder[R, E, A]](ctx: CaseClass[TDec, T]) extends OptionDecoder[R, E, T] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, T] =
      value match {
        case Toml.Table(map) =>
          ctx.constructMonadic { param =>
            map.get(param.label) match {
              case Some(memberValue) => param.typeclass.decode(resFactory)(memberValue)
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


  final class OptionDecoderDerivation[R, E] extends ProductDerivation[[A] =>> OptionDecoder[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionDecoder[R, E, T] =
      DerivationImplDecoder(ctx)
  }


  inline def derive[R, E, A](using Mirror.Of[A]): OptionDecoder[R, E, A] =
    new OptionDecoderDerivation[R, E].derivedMirror[A]

  // Toml
  given [R, E, A](using TomlCodec[A]): OptionDecoder[R, E, A] =
    summon[OptionCodec[R, E, A]]

  // Option
  private[options] open class OptionOptionDecoderBase[R, E, A](using OptionDecoder[R, E, A]) extends OptionDecoder[R, E, Option[A]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, Option[A]] =
      summon[OptionDecoder[R, E, A]].decode(resFactory)(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  }


  given[R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Option[A]] =
    OptionOptionDecoderBase()


  // Seq
  private[options] open class SeqOptionDecoderBase[R, E, A](using OptionDecoder[R, E, A]) extends OptionDecoder[R, E, Seq[A]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[R, E, A]].decode(resFactory))

        case _ => ZIO.fail("Expected array")
      }
  }


  given[R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Seq[A]] =
    SeqOptionDecoderBase()


  // DirectoryResource
  private[options] open class DirectoryResourceOptionDecoderBase[Res[-R2, +E2] <: Resource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]) extends OptionDecoder[R, E, DirectoryResource[R, E, Res]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, DirectoryResource[R, E, Res]] =
      value match {
        case Toml.String(str) =>
          ZIO.succeed((resFactory.directoryResource(str): DirectoryResource[R, E, BinaryResource]).decode[Res])

        case _ => ZIO.fail("Expected string")
      }
  }

  given directoryResourceDecoder[Res[-R2, +E2] <: Resource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, DirectoryResource[R, E, Res]] =
    DirectoryResourceOptionDecoderBase()


  // BinaryResource
  private[options] open class BinaryResourceOptionDecoderBase[Res[_, _], R, E](using BinaryResourceDecoder[Res, R, E]) extends OptionDecoder[R, E, Res[R, E]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, Res[R, E]] =
      value match {
        case Toml.String(str) =>
          ZIO.succeed(summon[BinaryResourceDecoder[Res, R, E]].decode(resFactory.binaryResource(str)))

        case _ => ZIO.fail("Expected string")
      }
  }

  given [Res[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, Res[R, E]] =
    BinaryResourceOptionDecoderBase()

}


