package dev.argon.plugins.source

import dev.argon.io.{BinaryResourceDecoder, DirectoryResource, ResourceFactory}
import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError
import dev.argon.util.*
import dev.argon.util.toml.Toml

import java.nio.charset.CharacterCodingException
import java.io.IOException

final case class SourceLibOptions[-R, +E, ContextOptions]
(
  name: NonEmptyList[String],
  spec: ArgonTubeSpecResource[R, E],
  sources: DirectoryResource[R, E, ArgonSourceCodeResource],
  platforms: ContextOptions,
)


object SourceLibOptions:

  given optionCodec[R, E >: SourceError, ContextOptions](using OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, SourceLibOptions[R, E, ContextOptions]] =
    OptionDecoder.derive

end SourceLibOptions

