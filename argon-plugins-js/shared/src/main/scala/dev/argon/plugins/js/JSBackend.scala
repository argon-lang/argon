package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.plugin.*
import dev.argon.options.OptionHandler
import zio.*

object JSBackend extends Backend[JSOptions, JSOutput] {
  override def emitModule[E](options: OptionHandler.WithRes[JSOptions, E], platforms: Set[Platform[E]])(tube: SerializedTube[E]): IO[E, JSOutput[E]] =
    ???
}
