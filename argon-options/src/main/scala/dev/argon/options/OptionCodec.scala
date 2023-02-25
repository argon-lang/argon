package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import zio.*

import scala.compiletime.summonInline


trait OptionCodec[R, E, A] extends OptionDecoder[R, E, A] {
  def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml]
}

object OptionCodec {


  inline def derive[R, E, A <: Product](using m: Mirror.ProductOf[A]): OptionCodec[R, E, A] =
    val tupleCodec = summonInline[TupleOptionCodec[R, E, m.MirroredElemTypes, m.MirroredElemLabels]]
    new OptionCodec[R, E, A] {
      override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
        value match {
          case value@Toml.Table(_) =>
            tupleCodec.decode(resFactory)(value).map(m.fromTuple)

          case _ => Left("Expected object")
        }

      override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml] =
        tupleCodec.encode(recorder)(Tuple.fromProductTyped(value))
    }
  end derive

  trait TupleOptionCodec[R, E, T <: Tuple, L <: Tuple] extends OptionDecoder.TupleOptionDecoder[R, E, T, L] {
    def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, Toml.Table]
  }

  given [R, E]: TupleOptionCodec[R, E, EmptyTuple, EmptyTuple] =
    new OptionDecoder.EmptyTupleOptionDecoderBase[R, E] with TupleOptionCodec[R, E, EmptyTuple, EmptyTuple] {
      override def encode(recorder: ResourceRecorder[R, E])(value: EmptyTuple): ZIO[R, E, Toml.Table] =
        ZIO.succeed(Toml.Table.empty)
    }

  given[R, E, H, T <: Tuple, Name <: String: ValueOf, TNames <: Tuple](using field: OptionFieldCodec[R, E, H], tail: TupleOptionCodec[R, E, T, TNames]): TupleOptionCodec[R, E, H *: T, Name *: TNames] =
    new OptionDecoder.ConsTupleOptionDecoderBase[R, E, H, T, Name, TNames] with TupleOptionCodec[R, E, H *: T, Name *: TNames] {
      override def encode(recorder: ResourceRecorder[R, E])(value: H *: T): ZIO[R, E, Toml.Table] =
        val (h *: t) = value
        for
          table <- tail.encode(recorder)(t)
          hValue <- field.encode(recorder)(h)
        yield hValue.fold(table) { v => Toml.Table(table.map.updated(summon[ValueOf[Name]].value, v)) }
      end encode
    }

  // Toml
  given tomlOptionCodec[R, E, A: TomlCodec]: OptionCodec[R, E, A] with
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
      summon[TomlCodec[A]].decode(value)

    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Toml] =
      ZIO.succeed(summon[TomlCodec[A]].encode(value))

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue
  end tomlOptionCodec

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
