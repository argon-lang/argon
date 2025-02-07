package dev.argon.io

import zio.stream.Stream
import esexpr.ESExprCodec
import esexpr.ESExpr

trait ESExprDecodedStreamResource[+E, A] extends ESExprStreamResource[E] {
  def decoded: Stream[E, A]
}

object ESExprDecodedStreamResource {
  trait Impl[E, A: ESExprCodec] extends ESExprDecodedStreamResource[E, A] {
    override def expr: Stream[E, ESExpr] =
      decoded.map(summon[ESExprCodec[A]].encode)
  }
}
