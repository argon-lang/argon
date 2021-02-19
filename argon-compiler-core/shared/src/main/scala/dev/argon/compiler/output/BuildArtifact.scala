package dev.argon.compiler.output

import dev.argon.compiler.CompStream

trait BuildArtifact {
  def asStream: CompStream[Byte]
}
