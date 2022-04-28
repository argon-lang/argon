package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.util.*
import zio.*

trait Backend[E, Options, Output] {
  def emitModule(options: Options, platforms: Set[Platform[E]])
                (tube: SerializedTube[E])
  : IO[E, Output]
}
