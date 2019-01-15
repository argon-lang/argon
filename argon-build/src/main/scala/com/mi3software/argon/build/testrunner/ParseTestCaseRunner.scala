package com.mi3software.argon.build.testrunner

import com.mi3software.argon.compiler.IOCompilation
import scalaz._
import scalaz.effect.IO

object ParseTestCaseRunner extends TestCaseRunner with TestCaseRunnerParsePhase {

  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    IOCompilation.compilationInstance.flatMap { implicit ioComp =>
      ioComp.getResult(parseTestCaseSource(testCase))
        .map {
          case \/-(_) => TestCaseResult.Success
          case -\/(errors) =>
            TestCaseResult.Failure(
              TestCaseActualResult.Errors(errors),
              TestCaseExpectedOutput("")
            )
        }
    }
}
