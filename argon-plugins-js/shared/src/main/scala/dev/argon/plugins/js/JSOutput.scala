package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.*

final case class JSOutput[E]
(
  module: JSProgramResource[E]
)

object JSOutput {
  object Module extends OutputInfo[JSProgramResource, JSOutput] {
    override val name: String = "module"
    override val description: String = "Compiled JS module"

    override def getValue[E](options: JSOutput[E]): JSProgramResource[E] =
      options.module
  }

  object Handler extends OutputHandler[JSOutput] {
    override val options: Set[OutputInfoAny[JSOutput]] =
      Set(Module)
  }
}
