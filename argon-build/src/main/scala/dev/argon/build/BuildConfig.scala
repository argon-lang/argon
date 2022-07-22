package dev.argon.build

import dev.argon.util.toml.{Toml, TomlCodec}
import dev.argon.io.*
import dev.argon.parser.SyntaxError
import dev.argon.options.*

import java.nio.charset.CharacterCodingException

final case class BuildConfig[R, E]
(
  tube: TubeOptions[R, E],
  backend: Map[String, Toml],
)

object BuildConfig:
  given [R, E]: OptionDecoder[R, E, BuildConfig[R, E]] =
    OptionDecoder.derive
end BuildConfig

final case class TubeOptions[R, E]
(
  name: String,
  path: Option[BinaryResource[R, E]],
  loader: Option[TubeLoaderOptions],
)

object TubeOptions:
  given [R, E]: OptionDecoder[R, E, TubeOptions[R, E]] =
    OptionDecoder.derive
end TubeOptions

final case class TubeLoaderOptions
(
  plugin: String,
  loader: String,
) derives TomlCodec

