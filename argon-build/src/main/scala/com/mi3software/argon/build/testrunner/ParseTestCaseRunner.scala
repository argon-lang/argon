package com.mi3software.argon.build.testrunner

import scalaz._
import scalaz.effect.IO

object ParseTestCaseRunner extends TestCaseRunner with TestCaseRunnerParsePhase {

  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    parseTestCaseSource(testCase)
      .run
      .map {
        case \/-(_) => TestCaseResult.Success
        case -\/(errors) =>
          TestCaseResult.Failure(
            TestCaseActualResult.Errors(errors),
            TestCaseExpectedOutput("")
          )
      }
}
