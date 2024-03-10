package dev.argon.plugins.lua

import dev.argon.esexpr.{Dictionary, keyword}
import dev.argon.options.OptionDecoder

final case class LuaOptions(
  @keyword externs: Dictionary[LuaExternImplementation] = Dictionary(Map.empty),
)

object LuaOptions {
  
  
  given [E]: OptionDecoder[E, LuaOptions] =
    OptionDecoder.derive[E, LuaOptions]
}
