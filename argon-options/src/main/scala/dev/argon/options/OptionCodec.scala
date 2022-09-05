package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import java.io.IOException
import zio.*


trait OptionCodec[R, E, A] extends OptionDecoder[E, A] {
  def encode(value: A): ZIO[ResourceRecorder & R, E, Toml]
  def skipForField(value: A): Boolean = false
}

object OptionCodec {



  // Derivation
  private class DerivationImplCodec[R, E >: IOException, T](ctx: CaseClass[[A] =>> OptionCodec[R, E, A], T]) extends OptionDecoder.DerivationImplDecoder[E, T, [A] =>> OptionCodec[R, E, A]](ctx) with OptionCodec[R, E, T] {
    override def encode(value: T): ZIO[ResourceRecorder & R, E, Toml] =
      ZIO.foreach(
        ctx.params
          .iterator
          .filterNot { param => param.typeclass.skipForField(param.deref(value)) }
          .toSeq
      ) { param =>
        param.typeclass.encode(param.deref(value)).map {
          param.label -> _
        }
      }
        .map { fields => Toml.Table(fields.toMap) }
  }

  final class OptionCodecDerivation[R, E >: IOException] extends ProductDerivation[[A] =>> OptionCodec[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionCodec[R, E, T] =
      DerivationImplCodec(ctx)
  }

  inline def derive[R, E >: IOException, A](using Mirror.Of[A]): OptionCodec[R, E, A] =
    new OptionCodecDerivation[R, E].derivedMirror[A]


  // Toml
  given tomlOptionCodec[R, E >: IOException, A: TomlCodec]: OptionCodec[R, E, A] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, A] =
      ZIO.fromEither(summon[TomlCodec[A]].decode(value))

    override def encode(value: A): ZIO[ResourceRecorder, E, Toml] =
      ZIO.succeed(summon[TomlCodec[A]].encode(value))

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue

    override def skipForField(value: A): Boolean =
      summon[TomlCodec[A]].skipForField(value)
  end tomlOptionCodec

  // Option
  given [R, E >: IOException, A](using OptionCodec[R, E, A], NotGiven[TomlCodec[A]]): OptionCodec[R, E, Option[A]] =
    new OptionDecoder.OptionOptionDecoderBase[E, A] with OptionCodec[R, E, Option[A]] {
      override def encode(value: Option[A]): ZIO[ResourceRecorder & R, E, Toml] =
        value.fold(ZIO.succeed(Toml.Table.empty))(summon[OptionCodec[R, E, A]].encode)

      override def skipForField(value: Option[A]): Boolean =
        value.isEmpty
    }

  // Seq
  given[R, E >: IOException, A](using OptionCodec[R, E, A], NotGiven[TomlCodec[A]]): OptionCodec[R, E, Seq[A]] =
    new OptionDecoder.SeqOptionDecoderBase[E, A] with OptionCodec[R, E, Seq[A]] {
      override def encode(value: Seq[A]): ZIO[ResourceRecorder & R, E, Toml] =
        ZIO.foreach(value)(summon[OptionCodec[R, E, A]].encode).map(Toml.Array.apply)
    }

  // DirectoryResource
  given directoryResourceCodec[Res[-R2, +E2] <: BinaryResource[R2, E2], R, E >: IOException](using BinaryResourceDecoder[Res, R, E]): OptionCodec[R, E, DirectoryResource[R, E, Res]] =
    new OptionDecoder.DirectoryResourceOptionDecoderBase[Res, R, E] with OptionCodec[R, E, DirectoryResource[R, E, Res]] {
      override def encode(value: DirectoryResource[R, E, Res]): ZIO[ResourceRecorder & R, E, Toml] =
        ZIO.serviceWithZIO[ResourceRecorder] { rr =>
          rr.recordDirectoryResource(value)
        }.map(Toml.String.apply)
    }


  // BinaryResource
  given [Res[R2, E2] <: BinaryResource[R2, E2], R, E >: IOException](using BinaryResourceDecoder[Res, R, E]): OptionCodec[R, E, Res[R, E]] =
    new OptionDecoder.BinaryResourceOptionDecoderBase[Res, R, E] with OptionCodec[R, E, Res[R, E]] {
      override def encode(value: Res[R, E]): ZIO[ResourceRecorder & R, E, Toml] =
        ZIO.serviceWithZIO[ResourceRecorder] { rr =>
          rr.recordBinaryResource(value)
        }.map(Toml.String.apply)
    }

}
