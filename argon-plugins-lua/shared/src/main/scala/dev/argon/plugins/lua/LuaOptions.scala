package dev.argon.plugins.lua

import dev.argon.options.OptionDecoder

final case class LuaOptions()

object LuaOptions {
  given [R, E]: OptionDecoder[R, E, LuaOptions] =
    OptionDecoder.derive[R, E, LuaOptions]
}
