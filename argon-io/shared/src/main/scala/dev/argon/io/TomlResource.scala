package dev.argon.io

import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.*
import zio.stream.*

import java.nio.charset.CharacterCodingException
import dev.argon.util.async.ErrorWrapper

trait TomlResource[+E] extends TextResource[E] {
  def asToml: IO[E, toml.Value.Tbl]
}

object TomlResource {
  final case class TomlDecodeError(address: toml.Parse.Address, message: toml.Parse.Message)
  object TomlDecodeError {
    def apply(error: toml.Parse.Error): TomlDecodeError =
      TomlDecodeError(error._1, error._2)
  }
  

  given [E >: CharacterCodingException | TomlDecodeError] => BinaryResourceDecoder[TomlResource, E]:
    override def decode(resource: BinaryResource[E]): TomlResource[E] =
      resource match {
        case resource: TomlResource[E] => resource
        case _ =>
          new TomlResource[E] {
            override def asToml: IO[E, toml.Value.Tbl] =
              resource.decode[TextResource]
                .asText
                .run(ZSink.mkString)
                .flatMap { s =>
                  ZIO.fromEither(toml.Toml.parse(s))
                    .mapError(TomlDecodeError.apply)
                }

            override def asText: Stream[E, String] =
              resource.decode[TextResource]
                .asText

            override def asBytes: Stream[E, Byte] =
              resource.asBytes

            override def fileName: Option[String] =
              resource.fileName
          }
      }
  end given
}
