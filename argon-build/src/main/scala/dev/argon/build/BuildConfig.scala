package dev.argon.build

import dev.argon.esexpr.*
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

final case class BuildConfig(
  @keyword plugins: Seq[String],
  @keyword tube: TubeOptions,
  @keyword output: OutputConfig,
  @keyword libraries: Seq[TubeOptions],
) derives ESExprCodec

final case class OutputConfig(
  @keyword options: ESExpr,
  @keyword dest: DeepStringDict,
) derives ESExprCodec

final case class TubeOptions(
  @keyword loader: TubeLoaderOptions,
  @keyword options: ESExpr,
) derives ESExprCodec

final case class TubeLoaderOptions(
  @keyword plugin: String,
  @keyword name: String,
) derives ESExprCodec

enum DeepStringDict derives ESExprCodec {
  @inlineValue
  case Str(s: String)
  
  @inlineValue
  case Dict(d: Dictionary[DeepStringDict])
}
