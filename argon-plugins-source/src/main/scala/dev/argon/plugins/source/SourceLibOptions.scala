package dev.argon.plugins.source

import dev.argon.io.{DirectoryResource, ResourceFactory}
import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError
import dev.argon.util.*
import java.nio.charset.CharacterCodingException
import java.io.IOException

final case class SourceLibOptions[-R, +E]
(
  name: NonEmptyList[String],
  spec: ArgonTubeSpecResource[R, E],
  sources: DirectoryResource[R, E, ArgonSourceCodeResource],
)


object SourceLibOptions:

  given optionDecoder[R, E >: CharacterCodingException | SyntaxError | IOException]: OptionDecoder[E, SourceLibOptions[Any, E]] =
    OptionDecoder.derive

end SourceLibOptions

