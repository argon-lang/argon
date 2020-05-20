package dev.argon

import shapeless._
import dev.argon.backend.Backend
import dev.argon.compiler.options.CompilerOptions

final case class CommandLineArguments
(
  command: ArgonCommand
)

sealed trait ArgonCommand

trait BuildCommand extends ArgonCommand {
  val backend: Backend
  val compilerOptions: CompilerOptions[Option, String]
  val backendOptions: backend.BackendOptions[Option, String]
  val outputOptions: backend.BackendOutputOptions[Option, String]
}
