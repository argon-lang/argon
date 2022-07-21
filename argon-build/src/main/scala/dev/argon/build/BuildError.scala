package dev.argon.build

sealed trait BuildError
final case class BuildConfigParseError(error: String) extends BuildError

