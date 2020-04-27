package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import cats.data.NonEmptyList

final case class TestCaseFailure(actualResult: TestCaseActualResult, expectedResult: TestCaseExpectedResult)

sealed trait TestCaseActualResult
sealed trait TestCaseError extends TestCaseActualResult
object TestCaseActualResult {
  case object NotExecuted extends TestCaseActualResult
  final case class Output(output: String) extends TestCaseActualResult
  final case class CompileErrors(errors: NonEmptyList[CompilationError]) extends TestCaseError
  final case class ExecutionError(ex: Throwable) extends TestCaseError
}
