package dev.argon.plugins.source

import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

final case class SourceOptions[R, E]()

object SourceOptions:

  given optionDecoder[R, E >: CharacterCodingException | SyntaxError]: OptionDecoder[R, E, SourceOptions[R, E]] =
    OptionDecoder.derive

end SourceOptions
