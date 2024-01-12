package dev.argon.plugins.wasm

import dev.argon.options.OptionDecoder

final case class WasmOptions()

object WasmOptions {
  given [R, E]: OptionDecoder[R, E, WasmOptions] =
    OptionDecoder.derive[R, E, WasmOptions]
}
