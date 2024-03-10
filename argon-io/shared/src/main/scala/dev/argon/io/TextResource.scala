package dev.argon.io

import zio.stream.*

import java.nio.charset.CharacterCodingException


trait TextResource[+E] extends BinaryResource[E] {
  def asText: ZStream[Any, E, String]
}

object TextResource:
  trait Impl[+E >: CharacterCodingException] extends TextResource[E]:
    override def asBytes: ZStream[Any, E, Byte] = ZPipeline.utf8Encode.apply(asText)
  end Impl

  given resourceDecoder: BinaryResourceDecoder[TextResource, CharacterCodingException] with
    def decode[E >: CharacterCodingException](resource: BinaryResource[E]): TextResource[E] =
      resource match {
        case resource: TextResource[E] => resource
        case _ => new TextResource.Impl[E] {
          override def asText: ZStream[Any, E, String] =
            resource.asBytes.via(ZPipeline.utf8Decode)

          override def fileName: Option[String] =
            resource.fileName
        }
      }
  end resourceDecoder

  def fromString(str: String): TextResource[CharacterCodingException] =
    new TextResource[CharacterCodingException] with Impl[CharacterCodingException] {
      override def asText: ZStream[Any, CharacterCodingException, String] = ZStream.succeed(str)
      override def fileName: Option[String] = None
    }

end TextResource
