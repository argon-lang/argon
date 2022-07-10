package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.plugin.*
import zio.*

object JSBackend extends Backend[JSOptions, JSOutput] {
  override def emitModule[E](options: JSOptions[Id, E], platforms: Set[Platform[E]])(tube: SerializedTube[E]): IO[E, JSOutput[E]] =
    ???
}
