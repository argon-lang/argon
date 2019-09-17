package dev.argon.webdemo

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError
import zio.Fiber

sealed trait ExecutionStatus

object ExecutionStatus {
  case object NotRun extends ExecutionStatus
  final case class Running(task: Fiber[Nothing, Unit]) extends ExecutionStatus
  final case class CompileFailed(errors: NonEmptyList[CompilationError]) extends ExecutionStatus
  final case class Failed(error: Throwable) extends ExecutionStatus
  final case class Completed(output: String) extends ExecutionStatus
}
