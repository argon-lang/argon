package dev.argon.plugins.source

import dev.argon.io.{DirectoryResource, ResourceFactory}
import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError
import dev.argon.util.*
import java.nio.charset.CharacterCodingException

case class SourceOptions[R, E]
(
  name: NonEmptyList[String],
  spec: ArgonTubeSpecResource[R, E],
  sources: DirectoryResource[R, E, ArgonSourceCodeResource],
)


object SourceOptions:

  given optionDecoder[R, E >: CharacterCodingException | SyntaxError]: OptionDecoder[R, E, SourceOptions[R, E]] =
    OptionDecoder.derive

end SourceOptions

