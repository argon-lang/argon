package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.util.*
import zio.*

trait Backend[Options[_[_[_]]], Output[_]] {
  def emitModule[E]
  (options: OptionHandler.WithRes[Options, E], platforms: Set[Platform[E]])
  (tube: SerializedTube[E])
  : IO[E, Output[E]]
}
