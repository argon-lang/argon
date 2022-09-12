package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import zio.*


trait OptionCodec[R, E, A] extends OptionDecoder[R, E, A] {
  def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml]
  def skipForField(value: A): Boolean = false
}

object OptionCodec {



  // Derivation
  private class DerivationImplCodec[R, E, T](ctx: CaseClass[[A] =>> OptionCodec[R, E, A], T]) extends OptionDecoder.DerivationImplDecoder[R, E, T, [A] =>> OptionCodec[R, E, A]](ctx) with OptionCodec[R, E, T] {
    override def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, Toml] =
      ZIO.foreach(
        ctx.params
          .iterator
          .filterNot { param => param.typeclass.skipForField(param.deref(value)) }
          .toSeq
      ) { param =>
        param.typeclass.encode(recorder)(param.deref(value)).map {
          param.label -> _
        }
      }
        .map { fields => Toml.Table(fields.toMap) }
  }

  final class OptionCodecDerivation[R, E] extends ProductDerivation[[A] =>> OptionCodec[R, E, A]] {
    override def join[T](ctx: CaseClass[Typeclass, T]): OptionCodec[R, E, T] =
      DerivationImplCodec(ctx)
  }

  inline def derive[R, E, A](using Mirror.Of[A]): OptionCodec[R, E, A] =
    new OptionCodecDerivation[R, E].derivedMirror[A]


  // Toml
  given tomlOptionCodec[R, E, A: TomlCodec]: OptionCodec[R, E, A] with
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): IO[String, A] =
      ZIO.fromEither(summon[TomlCodec[A]].decode(value))

    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml] =
      ZIO.succeed(summon[TomlCodec[A]].encode(value))

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue

    override def skipForField(value: A): Boolean =
      summon[TomlCodec[A]].skipForField(value)
  end tomlOptionCodec

  // Option
  given [R, E, A](using OptionCodec[R, E, A], NotGiven[TomlCodec[A]]): OptionCodec[R, E, Option[A]] =
    new OptionDecoder.OptionOptionDecoderBase[R, E, A] with OptionCodec[R, E, Option[A]] {
      override def encode(recorder: ResourceRecorder[R, E])(value: Option[A]): ZIO[R, E, Toml] =
        value.fold(ZIO.succeed(Toml.Table.empty))(summon[OptionCodec[R, E, A]].encode(recorder))

      override def skipForField(value: Option[A]): Boolean =
        value.isEmpty
    }

  // Seq
  given[R, E, A](using OptionCodec[R, E, A], NotGiven[TomlCodec[A]]): OptionCodec[R, E, Seq[A]] =
    new OptionDecoder.SeqOptionDecoderBase[R, E, A] with OptionCodec[R, E, Seq[A]] {
      override def encode(recorder: ResourceRecorder[R, E])(value: Seq[A]): ZIO[R, E, Toml] =
        ZIO.foreach(value)(summon[OptionCodec[R, E, A]].encode(recorder)).map(Toml.Array.apply)
    }

  // DirectoryResource
  given directoryResourceCodec[Res[-R2, +E2] <: BinaryResource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionCodec[R, E, DirectoryResource[R, E, Res]] =
    new OptionDecoder.DirectoryResourceOptionDecoderBase[Res, R, E] with OptionCodec[R, E, DirectoryResource[R, E, Res]] {
      override def encode(recorder: ResourceRecorder[R, E])(value: DirectoryResource[R, E, Res]): ZIO[R, E, Toml] =
        recorder.recordDirectoryResource(value)
          .map(Toml.String.apply)
    }


  // BinaryResource
  given [Res[R2, E2] <: BinaryResource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionCodec[R, E, Res[R, E]] =
    new OptionDecoder.BinaryResourceOptionDecoderBase[Res, R, E] with OptionCodec[R, E, Res[R, E]] {
      override def encode(recorder: ResourceRecorder[R, E])(value: Res[R, E]): ZIO[R, E, Toml] =
        recorder.recordBinaryResource(value)
          .map(Toml.String.apply)
    }

}
