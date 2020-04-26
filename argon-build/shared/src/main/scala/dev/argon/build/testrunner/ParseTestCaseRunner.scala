package dev.argon.build.testrunner

import zio.{IO, Runtime, ZIO}
import dev.argon.build._

object ParseTestCaseRunner extends TestCaseRunner with TestCaseRunnerParsePhase {

  override val name: String = "Parsing"

  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    parseTestCaseSource(testCase)
      .either
      .map {
        case Right(_) => TestCaseResult.Success
        case Left(errors) =>
          TestCaseResult.Failure(
            TestCaseActualResult.Errors(errors),
            TestCaseExpectedOutput("")
          )
      }
}
