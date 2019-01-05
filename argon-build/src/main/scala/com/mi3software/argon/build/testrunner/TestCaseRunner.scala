package com.mi3software.argon.build.testrunner

import scalaz.effect.IO

trait TestCaseRunner {
  def runTest(testCase: TestCase): IO[TestCaseResult]
}
