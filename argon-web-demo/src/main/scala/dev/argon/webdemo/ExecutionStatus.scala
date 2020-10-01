package dev.argon.webdemo

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError
import zio.{Cause, Fiber}

sealed trait ExecutionStatus

object ExecutionStatus {
  case object NotRun extends ExecutionStatus
  final case class Running(task: Fiber[Nothing, Unit]) extends ExecutionStatus
  final case class CompileFailed(errors: Cause[CompilationError]) extends ExecutionStatus
  final case class Failed(error: Throwable) extends ExecutionStatus
  final case class Completed(output: String) extends ExecutionStatus
}
