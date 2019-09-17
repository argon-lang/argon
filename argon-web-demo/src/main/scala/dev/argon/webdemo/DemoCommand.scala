package dev.argon.webdemo

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError

sealed trait DemoCommand

object DemoCommand {
  final case class Compile(code: String) extends DemoCommand
  final case class CompileFailureEvent(errors: NonEmptyList[CompilationError]) extends DemoCommand
  final case class ExecutionFailedEvent(error: Throwable) extends DemoCommand
  case object ExecutionCompleteEvent extends DemoCommand
  case object ClearOutput extends DemoCommand
  final case class AppendOutput(output: String) extends DemoCommand
}
