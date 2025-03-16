package dev.argon.build

import dev.argon.compiler.{TubeName, CompilerError}
import esexpr.ESExprCodec
import esexpr.parser.ESExprTextParseException
import dev.argon.util.*

sealed trait BuildError
final case class UnknownPlugin(pluginName: String) extends BuildError
final case class UnknownTube(tubeName: TubeName) extends BuildError
final case class DuplicateTube(tubeName: TubeName) extends BuildError
final case class UnknownOutput(outputName: Seq[String]) extends BuildError
final case class BuildFailed(errors: Seq[CompilerError]) extends BuildError
final case class InvalidTubeName(tubeName: String) extends BuildError


