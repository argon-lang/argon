package dev.argon.build.testrunner

import dev.argon.compiler._
import cats.data.NonEmptyList
import cats.implicits._

trait TestCaseRunner[-R] {

  val name: String

  def runTest(testCase: TestCase): RComp[R, TestCaseCompletedResult]

  def isResultExpected(actual: TestCaseActualResult, expected: TestCaseExpectedResult): Boolean =
    (actual, expected) match {
      case (TestCaseActualResult.NotExecuted, TestCaseExpectedOutput(_) | TestCaseExpectedAnyOutput) => true

      case (TestCaseActualResult.Output(_), TestCaseExpectedAnyOutput) => true
      case (TestCaseActualResult.Output(actualOutput), TestCaseExpectedOutput(expectedOutput)) =>
        normalizeOutput(actualOutput) === normalizeOutput(expectedOutput)

      case (TestCaseActualResult.Errors(errors), TestCaseExpectedError(errorName)) if errors.stripFailures.isEmpty =>
        NonEmptyList.fromList(errors.failures).exists(isExpectedError(_, errorName))

      case (TestCaseActualResult.Errors(_), _) => false
      case (_, TestCaseExpectedError(_)) => false
    }

  protected def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, Nil) =>
        realNames(error.getClass, Set.empty).exists { realName =>
          val expectedName = DiagnosticError.getClass.getName + errorName

          realName.startsWith(expectedName) &&
            (expectedName.length === realName.length || realName.charAt(expectedName.length) === '$')
        }

      case _ => false
    }

  @SuppressWarnings(Array("scalafix:DisableSyntax.==", "scalafix:DisableSyntax.null"))
  private def realNames(cls: Class[_], acc: Set[String]): Set[String] =
    if(cls == null || acc.contains(cls.getName)) acc
    else {
      realNames(cls.getSuperclass, acc + cls.getName)
    }

  protected def normalizeOutput(output: String): String =
    output.split("\n").map { _.trim }.filter { _.nonEmpty }.mkString("\n")

}
