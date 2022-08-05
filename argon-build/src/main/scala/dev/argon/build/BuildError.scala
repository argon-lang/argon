package dev.argon.build

import dev.argon.compiler.tube.TubeName
import dev.argon.util.*

sealed trait BuildError
final case class BuildConfigParseError(error: String) extends BuildError {
  override def toString: String = s"Error parsing build config: $error"
}
final case class UnknownPlugin(pluginName: String) extends BuildError
final case class UnknownTubeLoader(loaderOptions: TubeLoaderOptions) extends BuildError
final case class DuplicateTube(tubeName: TubeName) extends BuildError
final case class UnknownOutput(pluginName: String, outputName: Seq[String]) extends BuildError
final case class CouldNotLoadDeclarationTube(tubeName: TubeName) extends BuildError

