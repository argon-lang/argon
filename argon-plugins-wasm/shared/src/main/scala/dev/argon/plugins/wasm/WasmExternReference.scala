package dev.argon.plugins.wasm

import dev.argon.esexpr.{ESExprCodec, keyword}

final case class WasmExternReference(
  @keyword module: String,
  @keyword name: String,
) derives ESExprCodec
