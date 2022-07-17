package dev.argon.io

import zio.*
import zio.stream.*
import java.io.IOException

sealed trait Resource[+E]

trait DirectoryResource[+E] extends Resource[E] {
  def contents: Stream[E, DirectoryEntry[E]]
}

final case class DirectoryEntry[+E](name: String, resource: Resource[E])

trait BinaryResourceDecoder[Res[_]] {
  def decode[E >: ResourceDecodeException](resource: BinaryResource[E]): Res[E]
}


trait BinaryResource[+E] extends Resource[E] with BinaryResourcePlatformSpecific[E] {
  def asBytes: Stream[E, Byte]
}

object BinaryResource:
  given BinaryResourceDecoder[TextResource] with
    def decode[E >: ResourceDecodeException](resource: BinaryResource[E]): TextResource[E] =
      resource match {
        case resource: TextResource[E] => resource
        case _ => new TextResource[E] {
          override def asText: Stream[E, String] =
            resource.asBytes.via(ZPipeline.utf8Decode.mapError(ResourceDecodeException("Error decoding UTF-8 text", _)))
        }
      }
  end given
end BinaryResource

trait TextResource[+E] extends BinaryResource[E] {
  def asText: Stream[E, String]

  override def asBytes: Stream[E, Byte] = ZPipeline.utf8Encode.orDie.apply(asText)
}

object TextResource:
  given BinaryResourceDecoder[TextResource] with
    def decode[E >: ResourceDecodeException](resource: BinaryResource[E]): TextResource[E] =
      resource match {
        case resource: TextResource[E] => resource
        case _ => new TextResource[E] {
          override def asText: Stream[E, String] =
            resource.asBytes.via(ZPipeline.utf8Decode.mapError(ResourceDecodeException("Error decoding UTF-8 text", _)))
        }
      }
  end given
end TextResource

