package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import cats.data.NonEmptyList
import zio.Cause

final case class TestCaseFailure(actualResult: TestCaseActualResult, expectedResult: TestCaseExpectedResult)

sealed trait TestCaseActualResult
sealed trait TestCaseCompletedResult extends TestCaseActualResult

object TestCaseActualResult {
  case object NotExecuted extends TestCaseCompletedResult
  final case class Output(output: String) extends TestCaseCompletedResult
  final case class Errors(error: Cause[CompilationError]) extends TestCaseActualResult
}
