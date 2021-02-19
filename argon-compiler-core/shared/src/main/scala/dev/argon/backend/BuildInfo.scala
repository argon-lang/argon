package dev.argon.backend

import dev.argon.compiler.options.CompilerOptionID
import dev.argon.compiler.output.BuildArtifact
import dev.argon.options.Options
import shapeless.{Path => _, _}

trait BuildInfo {
  val backend: Backend
  val compilerOptions: Options[Id, CompilerOptionID]
  val backendOptions: Options[Id, backend.BackendOptionID]
  val outputFiles: Map[BuildArtifact, String]
}
