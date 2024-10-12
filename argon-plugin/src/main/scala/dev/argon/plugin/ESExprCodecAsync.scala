package dev.argon.plugin

import esexpr.{ESExpr, ESExprCodec}
import zio.*
import esexpr.ESExprCodec.DecodeError

trait ESExprCodecAsync[A] {
  def encode(value: A): UIO[ESExpr]
  def decode(expr: ESExpr): IO[ESExprCodec.DecodeError, A]
}

object ESExprCodecAsync {
  given [A: ESExprCodec]: ESExprCodecAsync[A] with
    override def encode(value: A): UIO[ESExpr] =
      ZIO.succeed(summon[ESExprCodec[A]].encode(value))

    override def decode(expr: ESExpr): IO[DecodeError, A] =
      ZIO.fromEither(summon[ESExprCodec[A]].decode(expr))
  end given
}
