package dev.argon.io

import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException
import dev.argon.util.async.ErrorWrapper
import toml.Value
import zio.IO

trait TomlDecodedResource[+E, +A] extends TomlResource[E] {
  def asDecoded: IO[E, A]
}

object TomlDecodedResource {
  given [E >: CharacterCodingException | TomlResource.TomlDecodeError, A: toml.Codec] => BinaryResourceDecoder[[E] =>> TomlDecodedResource[E, A], E]:
    override def decode(resource: BinaryResource[E]): TomlDecodedResource[E, A] =
      new TomlDecodedResource[E, A] {
        override def asDecoded: IO[E, A] =
          asToml.flatMap { tbl =>
            ZIO.fromEither(summon[toml.Codec[A]](tbl, Map(), 0))
              .mapError(TomlResource.TomlDecodeError.apply)
          }

        override def asToml: IO[E, Value.Tbl] =
          resource.decode[TomlResource].asToml

        override def asText: ZStream[Any, E, String] =
          resource.decode[TextResource].asText

        override def asBytes: ZStream[Any, E, Byte] =
          resource.asBytes

        override def fileName: Option[String] =
          resource.fileName
      }
  end given
}
