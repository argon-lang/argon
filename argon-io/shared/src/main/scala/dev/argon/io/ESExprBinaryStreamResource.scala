package dev.argon.io

import esexpr.*
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.*
import zio.stream.*

import dev.argon.util.async.ErrorWrapper
import java.io.IOException

trait ESExprBinaryStreamResource[+E] extends BinaryResource[E] with ESExprStreamResource[E]

object ESExprBinaryStreamResource {
  trait Impl[E] extends ESExprBinaryStreamResource[E] {
    override def asBytes: Stream[E, Byte] =
      val errorContext = ErrorWrapper.Context[E]
      import errorContext.given
      expr.viaFunction(ESExprBinaryEncoder.writeAll)
    end asBytes
  }

  given resourceDecoder: [E >: ESExprException | IOException] => BinaryResourceDecoder[ESExprBinaryStreamResource, E]:
    override def decode(resource: BinaryResource[E]): ESExprBinaryStreamResource[E] =
      resource match {
        case resource: ESExprBinaryStreamResource[E] => resource
        case _ =>
          new ESExprBinaryStreamResource[E] {
            override def expr: Stream[E, ESExpr] =
              val errorContext = ErrorWrapper.Context[E]
              import errorContext.given

              resource.asBytes.viaFunction(ESExprBinaryDecoder.readAll)
            end expr

            override def asBytes: ZStream[Any, E, Byte] =
              resource.asBytes

            override def fileName: Option[String] =
              resource.fileName
          }
      }
  end resourceDecoder
}
