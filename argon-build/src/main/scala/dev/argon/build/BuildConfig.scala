package dev.argon.build

import dev.argon.esexpr.*
import dev.argon.util.toml.{Toml, TomlCodec}
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

final case class BuildConfig(
  @keyword platforms: Seq[String],
  @keyword tube: TubeOptions,
  @keyword output: BuildOutput,
  @keyword libraries: Seq[TubeOptions],
) derives ESExprCodec

final case class BuildOutput(
  @dict output: Map[String, OutputConfig]
) derives ESExprCodec

final case class OutputConfig(
  @keyword options: ESExpr,
  @keyword dest: ESExpr,
) derives ESExprCodec

final case class TubeOptions(
  @keyword loader: TubeLoaderOptions,
  @keyword options: ESExpr,
  @keyword platforms: PlatformOptions,
) derives ESExprCodec

final case class PlatformOptions(
  @dict output: Map[String, ESExpr]
) derives ESExprCodec

final case class TubeLoaderOptions(
  @keyword plugin: String,
  @keyword name: String,
) derives ESExprCodec
