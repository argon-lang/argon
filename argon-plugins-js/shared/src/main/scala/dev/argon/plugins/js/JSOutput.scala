package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.*

final case class JSOutput[-R, +E]
(
  `package`: DirectoryResource[R, E, JSProgramResource],
)

object JSOutput {
  given [R, E]: OutputHandler[R, E, JSOutput[R, E]] =
    OutputHandler.derive
}
