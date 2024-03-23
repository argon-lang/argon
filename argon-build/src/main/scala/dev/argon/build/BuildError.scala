package dev.argon.build

import dev.argon.compiler.TubeName
import dev.argon.esexpr.ESExprCodec
import dev.argon.esexpr.parser.ESExprTextParseException
import dev.argon.util.*

sealed trait BuildError
final case class BuildConfigESExprParseError(error: ESExprTextParseException) extends BuildError {
  override def toString: String = s"Error parsing build config: ${error.getMessage()}"
}
final case class BuildConfigParseError(error: ESExprCodec.DecodeError) extends BuildError {
  override def toString: String = s"Error parsing build config: ${error.message} at ${error.path}"
}
final case class UnknownPlugin(pluginName: String) extends BuildError
final case class UnknownTubeLoader(loaderOptions: TubeLoaderOptions) extends BuildError
final case class UnknownTube(tubeName: TubeName) extends BuildError
final case class DuplicateTube(tubeName: TubeName) extends BuildError
final case class UnknownOutput(outputName: Seq[String]) extends BuildError
final case class BuildFailed(errorCount: Int) extends BuildError


