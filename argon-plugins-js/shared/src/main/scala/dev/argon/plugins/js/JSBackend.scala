package dev.argon.plugins.js

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.*
import dev.argon.plugin.*
import dev.argon.options.OptionHandler
import zio.*

object JSBackend extends Backend[JSOptions, JSOutput] {
  override def emitTube[E](context: Context)(options: OptionHandler.WithRes[JSOptions, E])(tube: ArTubeC with HasContext[context.type]): IO[E, JSOutput[E]] =
    ???
}
