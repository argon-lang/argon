package dev.argon.io

import esexpr.*
import esexpr.parser.ESExprTextReader
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.*
import zio.stream.*

import java.nio.charset.CharacterCodingException
import dev.argon.util.async.ErrorWrapper

trait ESExprTextResource[+E] extends TextResource[E] with ESExprResource[E]

object ESExprTextResource {
  given[E >: CharacterCodingException | ESExprException]: BinaryResourceDecoder[ESExprTextResource, E] with
    override def decode(resource: BinaryResource[E]): ESExprTextResource[E] =
      resource match {
        case resource: ESExprTextResource[E] => resource
        case _ =>
          new ESExprTextResource[E] {
            override def expr: IO[E, ESExpr] =
              resource.decode[TextResource]
                .asText
                .via(ESExprTextReader.read(fileName))
                .take(2)
                .runCollect
                .flatMap {
                  case Seq(elem) =>
                    ZIO.succeed(elem.value)

                  case exprs =>
                    ZIO.fail(ESExprDecodeException(s"Expected a single expression, got: $exprs"))
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
