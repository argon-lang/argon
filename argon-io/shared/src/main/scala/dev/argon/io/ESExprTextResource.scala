package dev.argon.io

import esexpr.*
import esexpr.parser.ESExprTextReader
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException

trait ESExprTextResource[E, A] extends TextResource[E] {
  protected def codec: ESExprCodec[A]

  def decoded: ZIO[Any, E, A]
}

object ESExprTextResource {
  given resourceCodec[A](using ESExprCodec[A]): BinaryResourceDecoder[[E] =>> ESExprTextResource[E, A], CharacterCodingException | ESExprException] with
    override def decode[E >: CharacterCodingException | ESExprException](resource: BinaryResource[E]): ESExprTextResource[E, A] =
      new ESExprTextResource[E, A] {
        override protected def codec: ESExprCodec[A] =
          summon[ESExprCodec[A]]

        override def decoded: ZIO[Any, E, A] =
          summon[BinaryResourceDecoder[TextResource, E]].decode(resource)
            .asText
            .via(ESExprTextReader.read(fileName))
            .runCollect
            .flatMap {
              case Seq(elem) =>
                ZIO.fromEither(codec.decode(elem.value))

              case exprs =>
                ZIO.fail(ESExprDecodeException(s"Expected a single expression, got: $exprs"))
            }

        override def asText: ZStream[Any, E, String] =
          summon[BinaryResourceDecoder[TextResource, E]].decode(resource)
            .asText

        override def asBytes: ZStream[Any, E, Byte] =
          resource.asBytes

        override def fileName: Option[String] =
          resource.fileName
      }
  end resourceCodec
}
