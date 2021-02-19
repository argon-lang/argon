package dev.argon.backend

import dev.argon.compiler._
import dev.argon.compiler.output.BuildArtifact

trait CompilationOutput {
  def getOutput(output: BuildArtifact): Option[CompStream[Byte]]
}
