package dev.argon.plugins.lua

import dev.argon.options.OptionDecoder

final case class LuaOutputOptions()

object LuaOutputOptions {
  given [E]: OptionDecoder[E, LuaOutputOptions] =
    OptionDecoder.derive[E, LuaOutputOptions]
}
