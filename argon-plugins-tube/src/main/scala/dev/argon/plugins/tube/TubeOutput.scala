package dev.argon.plugins.tube

import dev.argon.io.ZipFileResource
import dev.argon.options.OutputHandler

final case class TubeOutput[-R, +E]
(
  implementationModule: ZipFileResource[R, E],
  interfaceModule: ZipFileResource[R, E],
)

object TubeOutput:
  given [R, E]: OutputHandler[R, E, TubeOutput[R, E]] =
    OutputHandler.derive
end TubeOutput

