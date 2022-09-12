package dev.argon.plugins.tube

import dev.argon.compiler.Context
import dev.argon.io.ZipFileResource
import dev.argon.options.OptionDecoder

final case class TubeLibOptions[-R, +E, ContextOptions]
(
  tube: ZipFileResource[R, E],
)


object TubeLibOptions:

  given optionDecoder[R, E >: TubeError, ContextOptions](using OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, TubeLibOptions[R, E, ContextOptions]] =
    OptionDecoder.derive

end TubeLibOptions

