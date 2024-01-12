package dev.argon.plugins.wasm

import dev.argon.esexpr.ESExprCodec
import dev.argon.plugin.Extern

object WasmExtern extends Extern {
  override type Implementation = WasmExternImplementation
  override def implementationCodec: ESExprCodec[WasmExternImplementation] = summon[ESExprCodec[WasmExternImplementation]]

  override type Reference = WasmExternReference
  override def referenceCodec: ESExprCodec[WasmExternReference] = summon[ESExprCodec[WasmExternReference]]
}
