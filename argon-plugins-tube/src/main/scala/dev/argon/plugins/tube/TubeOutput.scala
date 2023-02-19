package dev.argon.plugins.tube

import dev.argon.io.ZipStreamResource
import dev.argon.options.OutputHandler

final case class TubeOutput[-R, +E]
(
  implementationModule: ZipStreamResource[R, E],
  interfaceModule: ZipStreamResource[R, E],
)

object TubeOutput:
  given [R, E]: OutputHandler[R, E, TubeOutput[R, E]] =
    OutputHandler.derive
end TubeOutput

