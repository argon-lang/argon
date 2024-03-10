package dev.argon.plugins.lua

import dev.argon.options.OptionDecoder

final case class LuaOptions()

object LuaOptions {
  given [E]: OptionDecoder[E, LuaOptions] =
    OptionDecoder.derive[E, LuaOptions]
}
