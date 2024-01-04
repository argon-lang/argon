package dev.argon.io

import zio.stream.*

import java.nio.charset.CharacterCodingException


trait TextResource[-R, +E] extends BinaryResource[R, E] {
  def asText: ZStream[R, E, String]
}

object TextResource:
  trait Impl[-R, +E >: CharacterCodingException] extends TextResource[R, E]:
    override def asBytes: ZStream[R, E, Byte] = ZPipeline.utf8Encode.apply(asText)
  end Impl

  given resourceDecoder: BinaryResourceDecoder[TextResource, Any, CharacterCodingException] with
    def decode[R <: Any, E >: CharacterCodingException](resource: BinaryResource[R, E]): TextResource[R, E] =
      resource match {
        case resource: TextResource[R, E] => resource
        case _ => new TextResource.Impl[R, E] {
          override def asText: ZStream[R, E, String] =
            resource.asBytes.via(ZPipeline.utf8Decode)

          override def fileName: Option[String] =
            resource.fileName
        }
      }
  end resourceDecoder

  def fromString(str: String): TextResource[Any, CharacterCodingException] =
    new TextResource[Any, CharacterCodingException] with Impl[Any, CharacterCodingException] {
      override def asText: ZStream[Any, CharacterCodingException, String] = ZStream.succeed(str)
      override def fileName: Option[String] = None
    }

end TextResource
