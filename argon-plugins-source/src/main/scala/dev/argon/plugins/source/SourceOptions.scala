package dev.argon.plugins.source

import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

final case class SourceOptions[-R, +E]()

object SourceOptions:

  given optionDecoder[E >: CharacterCodingException | SyntaxError]: OptionDecoder[E, SourceOptions[Any, E]] =
    OptionDecoder.derive

end SourceOptions
