package com.mi3software.argon.build.testrunner

trait TestCaseRunner {
  def runTest[F[_]: cats.effect.Sync](testCase: TestCase): F[TestCaseResult]
}
