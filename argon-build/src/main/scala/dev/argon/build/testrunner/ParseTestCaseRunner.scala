package dev.argon.build.testrunner

import dev.argon.compiler.IOCompilation
import scalaz._
import scalaz.zio.IO

object ParseTestCaseRunner extends TestCaseRunner with TestCaseRunnerParsePhase {

  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    IOCompilation.compilationInstance.flatMap { implicit ioComp =>
      ioComp.getResult(parseTestCaseSource(testCase))
        .map {
          case (_, \/-(_)) => TestCaseResult.Success
          case (_,-\/(errors)) =>
            TestCaseResult.Failure(
              TestCaseActualResult.Errors(errors),
              TestCaseExpectedOutput("")
            )
        }
    }
}
