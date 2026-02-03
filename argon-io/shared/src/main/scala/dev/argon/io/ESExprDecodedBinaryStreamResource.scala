package dev.argon.io

import esexpr.*
import dev.argon.io.{BinaryResource, BinaryResourceDecoder}
import zio.*
import zio.stream.*

import java.io.IOException
import izumi.reflect.Tag

trait ESExprDecodedBinaryStreamResource[+E, A](using val elementTag: Tag[A]) extends ESExprBinaryStreamResource[E] with ESExprDecodedStreamResource[E, A]

object ESExprDecodedBinaryStreamResource {
  trait Impl[E, A: ESExprCodec] extends ESExprDecodedBinaryStreamResource[E, A] with ESExprBinaryStreamResource.Impl[E] {
    override def expr: Stream[E, ESExpr] =
      decoded.map(summon[ESExprCodec[A]].encode)
  }

  given resourceDecoder: [E >: ESExprException | IOException, A: {ESExprCodec, Tag}] => BinaryResourceDecoder[[E1] =>> ESExprDecodedBinaryStreamResource[E1, A], E]:
    override def decode(resource: BinaryResource[E]): ESExprDecodedBinaryStreamResource[E, A] =
      resource match {
        case resource: ESExprDecodedBinaryStreamResource[E, ?] if summon[Tag[A]] =:= resource.elementTag =>
          resource.asInstanceOf[ESExprDecodedBinaryStreamResource[E, A]]

        case _ =>
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
      }
  end resourceDecoder
}
