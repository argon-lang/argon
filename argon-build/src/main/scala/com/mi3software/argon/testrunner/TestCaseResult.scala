package com.mi3software.argon.testrunner

import com.mi3software.argon.compiler.{CompilationMessage, CompilerInternalError}

sealed trait TestCaseResult
object TestCaseResult {
  object Success extends TestCaseResult
  final case class Failure(actualResult: TestCaseActualResult, expectedResult: TestCaseExpectedResult) extends TestCaseResult
  final case class Error(error: CompilerInternalError) extends TestCaseResult
}

sealed trait TestCaseVerifyResult
object TestCaseVerifyResult {
  object Success extends TestCaseVerifyResult
  object NoCompilationOutput extends TestCaseVerifyResult
  final case class Failure(validationErrors: String) extends TestCaseVerifyResult
  final case class Error(error: CompilerInternalError) extends TestCaseVerifyResult
}

sealed trait TestCaseActualResult
object TestCaseActualResult {
  final case class Output(output: String) extends TestCaseActualResult
  final case class Errors(errors: Vector[CompilationMessage]) extends TestCaseActualResult
}
