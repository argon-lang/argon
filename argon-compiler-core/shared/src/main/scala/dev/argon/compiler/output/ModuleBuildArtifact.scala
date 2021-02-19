package dev.argon.compiler.output

import zio.UIO

trait ModuleBuildArtifact extends BuildArtifact {
  def serialized: UIO[ArgonModuleSerialized]
}
