package dev.argon.io

import dev.argon.esexpr.parser.{ESExprLexer, ESExprParser, ESExprTextReader}
import dev.argon.esexpr.{ESExprCodec, ESExprException, ESExprDecodeException}
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException

trait ESExprTextResource[R, E, A] extends TextResource[R, E] {
  protected def codec: ESExprCodec[A]

  def decoded: ZIO[R, E, A]
}

object ESExprTextResource {
  given resourceDecoder[A](using ESExprCodec[A]): BinaryResourceDecoder[[R, E] =>> ESExprTextResource[R, E, A], Any, CharacterCodingException | ESExprException] with
    override def decode[R <: Any, E >: CharacterCodingException | ESExprException](resource: BinaryResource[R, E]): ESExprTextResource[R, E, A] =
      new ESExprTextResource[R, E, A] {
        override protected def codec: ESExprCodec[A] =
          summon[ESExprCodec[A]]

        override def decoded: ZIO[R, E, A] =
          summon[BinaryResourceDecoder[TextResource, R, E]].decode(resource)
            .asText
            .pipeThroughChannel(ESExprTextReader.read(fileName))
            .runCollect
            .flatMap {
              case Seq(elem) =>
                ZIO.fromEither(codec.decode(elem.value))
                  .mapError(ESExprDecodeException(_))

              case exprs =>
                ZIO.fail(ESExprDecodeException(s"Expected a single expression, got: $exprs"))
            }

        override def asText: ZStream[R, E, String] =
          summon[BinaryResourceDecoder[TextResource, R, E]].decode(resource)
            .asText

        override def asBytes: ZStream[R, E, Byte] =
          resource.asBytes

        override def fileName: Option[String] =
          resource.fileName
      }
  end resourceDecoder
}
