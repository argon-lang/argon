package dev.argon.build

import dev.argon.esexpr.*
import dev.argon.util.toml.{Toml, TomlCodec}
import dev.argon.io.*
import dev.argon.parser.SyntaxError
import dev.argon.options.*

import java.nio.charset.CharacterCodingException

final case class BuildConfig(
  @keyword plugins: Seq[String],
  @keyword tube: TubeOptions,
  @keyword output: BuildOutput,
  @keyword libraries: Seq[TubeOptions],
) derives ESExprCodec

final case class BuildOutput(
  @dict output: Map[String, ESExpr]
) derives ESExprCodec

final case class TubeOptions(
  @keyword loader: TubeLoaderOptions,
  @keyword options: ESExpr,
) derives ESExprCodec

final case class TubeLoaderOptions(
  @keyword plugin: String,
  @keyword name: String,
) derives ESExprCodec

final case class PluginOptions(
  @keyword options: Option[ESExpr],
  @keyword output: Option[ESExpr],
) derives ESExprCodec

