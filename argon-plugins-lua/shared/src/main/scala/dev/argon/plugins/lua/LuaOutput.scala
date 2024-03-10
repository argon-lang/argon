package dev.argon.plugins.lua

import dev.argon.esexpr.keyword
import dev.argon.options.OutputHandler

final case class LuaOutput[E](
  @keyword chunk: LuaChunkResource[E] 
)

object LuaOutput {
  given [E]: OutputHandler[E, LuaOutput[E]] =
    OutputHandler.derive[E, LuaOutput[E]]
}
