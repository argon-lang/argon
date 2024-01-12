package dev.argon.plugins.wasm

import dev.argon.options.OutputHandler

final case class WasmOutput()

object WasmOutput {
  given [R, E]: OutputHandler[R, E, WasmOutput] =
    OutputHandler.derive[R, E, WasmOutput]
}
