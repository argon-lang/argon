package dev.argon.build.testrunner

import zio.{IO, Runtime, UIO, ZIO}
import dev.argon.build._
import cats.implicits._

object ParseTestCaseRunner extends TestCaseRunnerParsePhase[Any] {

  override val name: String = "Parsing"

  override def runTest(testCase: TestCase): UIO[TestCaseActualResult] =
    parseTestCaseSource(testCase)
      .as(TestCaseActualResult.NotExecuted)
      .catchAll(IO.succeed(_))

  override def isResultExpected(actual: TestCaseActualResult, expected: TestCaseExpectedResult): Boolean =
    expected match {
      case TestCaseExpectedError("SyntaxCompilerError") =>
        super.isResultExpected(actual, expected)

      case _ =>
        super.isResultExpected(actual, TestCaseExpectedAnyOutput)
    }
}
