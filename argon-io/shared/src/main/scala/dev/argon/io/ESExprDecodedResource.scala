package dev.argon.io

import zio.IO
import esexpr.ESExprCodec
import esexpr.ESExpr

trait ESExprDecodedResource[E, A] extends ESExprResource[E] {
  def decoded: IO[E, A]
}

object ESExprDecodedResource {
  trait Impl[E, A: ESExprCodec] extends ESExprDecodedResource[E, A] {
    override def expr: IO[E, ESExpr] =
      decoded.map(summon[ESExprCodec[A]].encode)
  }
}
