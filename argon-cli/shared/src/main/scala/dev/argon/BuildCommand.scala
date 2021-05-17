package dev.argon

import dev.argon.backend.Backend
import dev.argon.compiler.options.{CompilerOptionID, GeneralOutputOptionID}
import dev.argon.options.Options

trait BuildCommand {
  val backend: Backend

  val compilerOptions: Options[Option, CompilerOptionID]
  val backendOptions: Options[Option, backend.BackendOptionID]

  val generalOutputOptions: Options[Option, GeneralOutputOptionID]
  val backendOutputOptions: Options[Option, backend.OutputOptionID]
}
