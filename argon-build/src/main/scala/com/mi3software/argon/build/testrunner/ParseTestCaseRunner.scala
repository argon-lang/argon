package com.mi3software.argon.build.testrunner

import scalaz._
import scalaz.effect.IO

import TestCaseRunner._

object ParseTestCaseRunner extends TestCaseRunner {

  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    parseInput(testCase)
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
