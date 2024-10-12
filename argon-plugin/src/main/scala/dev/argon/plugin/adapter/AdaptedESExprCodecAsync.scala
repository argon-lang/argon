package dev.argon.plugin.adapter

import esexpr.*
import dev.argon.plugin.scalaApi.EsexprCodec
import dev.argon.plugin.ESExprCodecAsync
import zio.*

final class AdaptedESExprCodecAsync[A, W <: ExternWrapper[A]](codec: UIO[EsexprCodec[A]])(makeWrapper: A => W) extends ESExprCodecAsync[W] {
  def encode(value: W): UIO[ESExpr] =
    codec.flatMap(_.encode(value.inner))

  def decode(expr: ESExpr): IO[ESExprCodec.DecodeError, W] =
    codec.flatMap(_.decode(expr))
      .map(makeWrapper)
      .mapError(error => ESExprCodec.DecodeError(
        error.getMessage(),
        error.information
      ))
}
