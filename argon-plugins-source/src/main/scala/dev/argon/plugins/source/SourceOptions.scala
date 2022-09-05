package dev.argon.plugins.source

import dev.argon.options.OptionCodec
import dev.argon.parser.SyntaxError
import java.io.IOException

import java.nio.charset.CharacterCodingException

final case class SourceOptions[-R, +E]()

object SourceOptions:

  given optionDecoder[R, E >: CharacterCodingException | SyntaxError | IOException]: OptionCodec[R, E, SourceOptions[R, E]] =
    OptionCodec.derive

end SourceOptions
