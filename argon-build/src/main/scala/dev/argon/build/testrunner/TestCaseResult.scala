package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import scalaz.NonEmptyList

sealed trait TestCaseResult
object TestCaseResult {
  object Success extends TestCaseResult
  final case class Failure(actualResult: TestCaseActualResult, expectedResult: TestCaseExpectedResult) extends TestCaseResult
  final case class ExecutionError(ex: Throwable) extends TestCaseResult
}

sealed trait TestCaseActualResult
object TestCaseActualResult {
  final case class Output(output: String) extends TestCaseActualResult
  final case class Errors(errors: NonEmptyList[CompilationError]) extends TestCaseActualResult
}
