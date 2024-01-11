package dev.argon.plugins.source

import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError
import java.io.IOException

import java.nio.charset.CharacterCodingException

final case class SourceOptions[PlatformOptions](
  platform: PlatformOptions,
)

object SourceOptions:

  given optionDecoder[R, E >: CharacterCodingException | SyntaxError | IOException, PlatformOptions: [A] =>> OptionDecoder[R, E, A]]: OptionDecoder[R, E, SourceOptions[PlatformOptions]] =
    OptionDecoder.derive[R, E, SourceOptions[PlatformOptions]]

end SourceOptions
