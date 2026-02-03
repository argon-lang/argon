package dev.argon.io

import zio.stream.*

import java.nio.charset.CharacterCodingException


trait TextResource[+E] extends BinaryResource[E] {
  def asText: Stream[E, String]
}

object TextResource:
  trait Impl[+E >: CharacterCodingException] extends TextResource[E]:
    override def asBytes: Stream[E, Byte] = ZPipeline.utf8Encode.apply(asText)
  end Impl

  given resourceDecoder: [E >: CharacterCodingException] => BinaryResourceDecoder[TextResource, E]:
    def decode(resource: BinaryResource[E]): TextResource[E] =
      resource match {
        case resource: TextResource[E] => resource
        case _ => new TextResource[E] {
          override def asText: Stream[E, String] =
            resource.asBytes.via(ZPipeline.utf8Decode)

          override def asBytes: Stream[E, Byte] =
            resource.asBytes

          override def fileName: Option[String] =
            resource.fileName
        }
      }
  end resourceDecoder

  def fromString(str: String): TextResource[CharacterCodingException] =
    new TextResource[CharacterCodingException] with Impl[CharacterCodingException] {
      override def asText: Stream[CharacterCodingException, String] = ZStream.succeed(str)
      override def fileName: Option[String] = None
    }

end TextResource
