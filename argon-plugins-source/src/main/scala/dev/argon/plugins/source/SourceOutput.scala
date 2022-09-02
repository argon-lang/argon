package dev.argon.plugins.source

import dev.argon.options.OutputHandler

final case class SourceOutput[-R, +E]()

object SourceOutput:
  given [R, E]: OutputHandler[R, E, SourceOutput[R, E]] =
    OutputHandler.derive
end SourceOutput


