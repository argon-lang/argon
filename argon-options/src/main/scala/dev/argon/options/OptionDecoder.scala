package dev.argon.options

import esexpr.{ESExpr, ESExprCodec}
import zio.*
import esexpr.ESExprCodec
import dev.argon.io.ResourceReader
import java.io.IOException

trait OptionDecoder[A] {
  def decode(expr: ESExpr): ZIO[ResourceReader, ESExprCodec.DecodeError, A]
}

