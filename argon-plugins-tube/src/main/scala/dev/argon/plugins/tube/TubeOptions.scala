package dev.argon.plugins.tube

import dev.argon.options.OptionCodec
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

final case class TubeOptions[-R, +E]()

object TubeOptions:

  given optionCodec[R, E >: TubeError]: OptionCodec[R, E, TubeOptions[R, E]] =
    OptionCodec.derive

end TubeOptions

