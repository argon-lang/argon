package dev.argon.plugins.wasm

import dev.argon.options.OptionDecoder

case class WasmOutputOptions()

object WasmOutputOptions {
  given [R, E]: OptionDecoder[R, E, WasmOutputOptions] =
    OptionDecoder.derive[R, E, WasmOutputOptions]
}
