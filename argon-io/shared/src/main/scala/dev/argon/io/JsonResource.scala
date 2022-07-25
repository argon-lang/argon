package dev.argon.io

import zio.*
import zio.stream.*
import zio.json.*

import java.nio.charset.CharacterCodingException

abstract class JsonResource[R, E] extends TextResource[R, E] {
  def decode[A: JsonDecoder]: ZIO[R, E, A]
}

object JsonResource {

  final case class DecodeError(value: String)

  given BinaryResourceDecoder[JsonResource, Any, CharacterCodingException | DecodeError] with
    override def decode[R, E >: CharacterCodingException | DecodeError](resource: BinaryResource[R, E]): JsonResource[R, E] =
      new JsonResource[R, E] with TextResource.Impl[R, E] {
        override def decode[A: JsonDecoder]: ZIO[R, E, A] =
          asText.runCollect.flatMap { text =>
            ZIO.fromEither(summon[JsonDecoder[A]].decodeJson(text.mkString))
              .mapError(DecodeError.apply)
          }

        override def asText: ZStream[R, E, String] =
          summon[BinaryResourceDecoder[TextResource, R, E]].decode(resource).asText

        override def fileName: Option[String] = resource.fileName
      }

  end given


}
