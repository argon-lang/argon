package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.util.*
import zio.*

trait Backend[Options[_[_[_]]], Output[_]] {
  def emitTube[E]
  (context: Context)
  (options: OptionHandler.WithRes[Options, E])
  (tube: ArTubeC with HasContext[context.type])
  : IO[E, Output[E]]
}
