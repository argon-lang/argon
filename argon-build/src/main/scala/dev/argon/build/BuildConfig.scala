package dev.argon.build

import dev.argon.util.toml.{Toml, TomlCodec}
import dev.argon.io.*
import dev.argon.parser.SyntaxError
import dev.argon.options.*

import java.nio.charset.CharacterCodingException

final case class BuildConfig
(
  tube: TubeOptions,
  plugin: Map[String, PluginOptions],
  libraries: Seq[LibOptions],
) derives TomlCodec

final case class TubeOptions
(
  loader: Option[TubeLoaderOptions],
  options: Toml,
) derives TomlCodec

final case class LibOptions
(
  loader: TubeLoaderOptions,
  options: Toml,
) derives TomlCodec

final case class TubeLoaderOptions
(
  plugin: String,
  loader: String,
) derives TomlCodec

final case class PluginOptions
(
  options: Option[Toml],
  output: Option[Toml],
) derives TomlCodec

