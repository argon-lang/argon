package dev.argon.options

import dev.argon.io.{ResourceFactory, ResourceRecorder}
import dev.argon.util.toml.Toml
import zio.ZIO

trait OptionFieldCodec[R, E, A] extends OptionFieldDecoder[R, E, A] {
  def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Option[Toml]]
}

object OptionFieldCodec {
  given[R, E, A] (using codec: OptionCodec[R, E, A]): OptionFieldCodec[R, E, A] with
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
      codec.decode(resFactory)(value)

    override def defaultValue: Option[A] = codec.defaultValue

    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Option[Toml]] =
      codec.encode(recorder)(value).asSome
  end given

  given[R, E, A] (using codec: OptionCodec[R, E, A]): OptionFieldCodec[R, E, Option[A]] with
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, Option[A]] =
      codec.decode(resFactory)(value).map(Some.apply)

    override def defaultValue: Option[Option[A]] = Some(None)

    override def encode(recorder: ResourceRecorder[R, E])(value: Option[A]): ZIO[R, E, Option[Toml]] =
      ZIO.foreach(value)(codec.encode(recorder))
  end given
}
