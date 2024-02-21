package dev.argon.plugins.lua

import dev.argon.esexpr.keyword
import dev.argon.options.OutputHandler

final case class LuaOutput[R, E](
  @keyword chunk: LuaChunkResource[R, E] 
)

object LuaOutput {
  given [R, E]: OutputHandler[R, E, LuaOutput[R, E]] =
    OutputHandler.derive[R, E, LuaOutput[R, E]]
}
