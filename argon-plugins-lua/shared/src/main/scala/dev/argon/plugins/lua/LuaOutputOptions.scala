package dev.argon.plugins.lua

import dev.argon.options.OptionDecoder

case class LuaOutputOptions()

object LuaOutputOptions {
  given [R, E]: OptionDecoder[R, E, LuaOutputOptions] =
    OptionDecoder.derive[R, E, LuaOutputOptions]
}
