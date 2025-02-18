package dev.argon.io

import esexpr.*
import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import zio.*
import zio.stream.*

import dev.argon.util.async.ErrorWrapper
import java.io.IOException

trait ESExprDecodedBinaryStreamResource[+E, A] extends ESExprBinaryStreamResource[E] with ESExprDecodedStreamResource[E, A]

object ESExprDecodedBinaryStreamResource {
  trait Impl[E, A: ESExprCodec] extends ESExprDecodedBinaryStreamResource[E, A] with ESExprBinaryStreamResource.Impl[E] {
    override def expr: Stream[E, ESExpr] =
      decoded.map(summon[ESExprCodec[A]].encode)
  }

  given resourceDecoder: [E >: ESExprException | IOException, A: ESExprCodec] => BinaryResourceDecoder[[E1] =>> ESExprDecodedBinaryStreamResource[E1, A], E]:
    override def decode(resource: BinaryResource[E]): ESExprDecodedBinaryStreamResource[E, A] =
      new ESExprDecodedBinaryStreamResource[E, A] {
        override def decoded: Stream[E, A] =
          expr.mapZIO { elem =>
            ZIO.fromEither(summon[ESExprCodec[A]].decode(elem))
          }

        override def expr: Stream[E, ESExpr] =
          resource.decode[ESExprBinaryStreamResource].expr

        override def asBytes: ZStream[Any, E, Byte] =
          resource.asBytes

        override def fileName: Option[String] =
          resource.fileName
      }
  end resourceDecoder
}
