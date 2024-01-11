package dev.argon.plugins.source

import dev.argon.options.OutputHandler

final case class SourceOutput()

object SourceOutput:
  given [R, E]: OutputHandler[R, E, SourceOutput] =
    OutputHandler.derive
end SourceOutput


