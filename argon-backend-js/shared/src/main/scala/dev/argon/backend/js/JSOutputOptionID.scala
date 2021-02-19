package dev.argon.backend.js

import dev.argon.options.OptionID
import dev.argon.compiler.output.{BuildArtifact, TextBuildArtifact}

sealed trait JSOutputOptionID extends OptionID {
  override type ElementType <: BuildArtifact
}

object JSOutputOptionID {
  case object JSModule extends JSOutputOptionID {
    override type ElementType = TextBuildArtifact
  }
}
