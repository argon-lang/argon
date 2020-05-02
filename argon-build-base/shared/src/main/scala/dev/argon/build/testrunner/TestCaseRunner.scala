package dev.argon.build.testrunner

import dev.argon.compiler.{CompilationError, ErrorList}
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import zio._
import dev.argon.build._

trait TestCaseRunner[-R] {

  val name: String

  def runTest(testCase: TestCase): URIO[R, TestCaseActualResult]

  def isResultExpected(actual: TestCaseActualResult, expected: TestCaseExpectedResult): Boolean =
    (actual, expected) match {
      case (TestCaseActualResult.NotExecuted, TestCaseExpectedOutput(_) | TestCaseExpectedAnyOutput) => true

      case (TestCaseActualResult.Output(_), TestCaseExpectedAnyOutput) => true
      case (TestCaseActualResult.Output(actualOutput), TestCaseExpectedOutput(expectedOutput)) =>
        normalizeOutput(actualOutput) === normalizeOutput(expectedOutput)

      case (TestCaseActualResult.CompileErrors(errors), TestCaseExpectedError(errorName)) =>
        isExpectedError(errors, errorName)

      case (TestCaseActualResult.CompileErrors(_), TestCaseExpectedOutput(_) | TestCaseExpectedAnyOutput) => false
      case (_, TestCaseExpectedError(_)) => false
      case (TestCaseActualResult.ExecutionError(_), _) => false
    }

  protected def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, Nil) =>
        val realName = error.getClass.getName
        val expectedName = CompilationError.getClass.getName + errorName

        realName.startsWith(expectedName) &&
          (expectedName.length === realName.length || realName.charAt(expectedName.length) === '$')

      case _ => false
    }

  protected def normalizeOutput(output: String): String =
    output.split("\n").map { _.trim }.filter { _.nonEmpty }.mkString("\n")

  protected final def compilationFailureResult(errors: ErrorList): TestCaseError =
    TestCaseActualResult.CompileErrors(errors)

}
