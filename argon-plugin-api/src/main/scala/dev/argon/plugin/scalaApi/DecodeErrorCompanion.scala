package dev.argon.plugin.scalaApi

import esexpr.ESExprCodec
import dev.argon.util.{*, given}

trait DecodeErrorCompanion {
  def fromCodecError(error: ESExprCodec.DecodeError): DecodeError =
    DecodeError(error.path, error.message)

  def toCodecError(error: DecodeError): ESExprCodec.DecodeError =
    val msg = error.getMessage()
    ESExprCodec.DecodeError(if msg == null then "" else msg, error.information)
  end toCodecError
}
