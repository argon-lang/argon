package dev.argon.backend.js

import dev.argon.options.{OptionID, SingleFile, TypedOptionID}
import dev.argon.compiler.output.{BuildArtifact, TextBuildArtifact}

sealed trait JSOutputOptionID extends OptionID {
  override type ElementType <: BuildArtifact
  override type Decoded[_] = SingleFile
}

object JSOutputOptionID {
  type AsFile[_] = SingleFile
  case object JSModule extends TypedOptionID[AsFile, TextBuildArtifact] with JSOutputOptionID
}
