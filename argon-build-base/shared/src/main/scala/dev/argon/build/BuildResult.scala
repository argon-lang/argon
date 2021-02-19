package dev.argon.build

import cats.Id
import dev.argon.backend.Backend
import dev.argon.compiler.core.Context
import dev.argon.compiler.options.GeneralOutputOptionID
import dev.argon.options.Options

trait BuildResult {
  val backend: Backend
  val context: Context.Aux[backend.type]

  val generalOutput: Options[Id, GeneralOutputOptionID]
  val backendOutput: Options[Id, backend.OutputOptionID]
}

object BuildResult {
  type Aux[TBackend <: Backend] = BuildResult { val backend: TBackend }
}
