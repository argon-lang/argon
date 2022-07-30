package dev.argon.build

import dev.argon.util.toml.{Toml, TomlCodec}
import dev.argon.io.*
import dev.argon.parser.SyntaxError
import dev.argon.options.*

import java.nio.charset.CharacterCodingException

final case class BuildConfig
(
  plugins: Seq[String],
  tube: TubeOptions,
  output: Map[String, Toml],
  libraries: Seq[TubeOptions],
) derives TomlCodec

final case class TubeOptions
(
  loader: TubeLoaderOptions,
  options: Toml,
) derives TomlCodec

final case class TubeLoaderOptions
(
  plugin: String,
  name: String,
) derives TomlCodec

final case class PluginOptions
(
  options: Option[Toml],
  output: Option[Toml],
) derives TomlCodec

