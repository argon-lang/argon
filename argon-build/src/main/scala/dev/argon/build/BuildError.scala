package dev.argon.build

import dev.argon.compiler.tube.TubeName

sealed trait BuildError
final case class BuildConfigParseError(error: String) extends BuildError
final case class UnknownPlugin(pluginName: String) extends BuildError
final case class UnknownTubeLoader(loaderOptions: TubeLoaderOptions) extends BuildError
final case class DuplicateTube(tubeName: TubeName) extends BuildError
final case class UnknownOutput(pluginName: String, outputName: Seq[String]) extends BuildError

