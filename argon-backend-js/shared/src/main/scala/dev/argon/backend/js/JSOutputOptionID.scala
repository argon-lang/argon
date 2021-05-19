package dev.argon.backend.js

import dev.argon.backend.Backend.AsFile
import dev.argon.options.{OptionID, OptionIDBase, OptionInfo, SingleFile}
import dev.argon.compiler.output.{BuildArtifact, TextBuildArtifact}

sealed trait JSOutputOptionID extends OptionID {
  override type ElementType <: BuildArtifact
  override type Decoded[_] = SingleFile
}

object JSOutputOptionID {
  case object JSModule extends OptionIDBase[AsFile, TextBuildArtifact] with JSOutputOptionID {
    override val info: OptionInfo[TextBuildArtifact] = OptionInfo("output.js.module", "The compiled JS module file")
  }
}
