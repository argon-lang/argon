package dev.argon.plugins.tube

import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

final case class TubeOptions[-R, +E]()

object TubeOptions:

  given optionDecoder[E >: CharacterCodingException | SyntaxError]: OptionDecoder[E, TubeOptions[Any, E]] =
    OptionDecoder.derive

end TubeOptions

