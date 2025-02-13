package dev.argon.io

import esexpr.*
import esexpr.parser.ESExprTextReader
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException
import dev.argon.util.async.ErrorWrapper
import zio.IO

trait ESExprDecodedTextResource[+E, +A] extends ESExprTextResource[E] with ESExprDecodedResource[E, A]

object ESExprDecodedTextResource {
  given[E >: CharacterCodingException | ESExprException, A: ESExprCodec]: BinaryResourceDecoder[[E] =>> ESExprDecodedTextResource[E, A], E] with
    override def decode(resource: BinaryResource[E]): ESExprDecodedTextResource[E, A] =
      new ESExprDecodedTextResource[E, A] {
        override def decoded: ZIO[Any, E, A] =
          expr.flatMap { e => ZIO.fromEither(summon[ESExprCodec[A]].decode(e)) }

        override def expr: IO[E, ESExpr] =
          resource.decode[ESExprTextResource].expr

        override def asText: ZStream[Any, E, String] =
          resource.decode[TextResource].asText

        override def asBytes: ZStream[Any, E, Byte] =
          resource.asBytes

        override def fileName: Option[String] =
          resource.fileName
      }
  end given
}
