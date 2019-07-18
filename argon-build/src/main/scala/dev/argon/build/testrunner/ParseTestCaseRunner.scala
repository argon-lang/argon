package dev.argon.build.testrunner

import dev.argon.compiler.IOCompilation
import zio.blocking.Blocking
import zio.{IO, Runtime, ZIO}

object ParseTestCaseRunner extends TestCaseRunner with TestCaseRunnerParsePhase {

  override def runTest(testCase: TestCase): ZIO[Blocking, Throwable, TestCaseResult] =
    IOCompilation.compilationInstance[Blocking].flatMap { implicit ioComp =>
      ioComp.getResult(parseTestCaseSource(testCase))
        .map {
          case (_, Right(_)) => TestCaseResult.Success
          case (_, Left(errors)) =>
            TestCaseResult.Failure(
              TestCaseActualResult.Errors(errors),
              TestCaseExpectedOutput("")
            )
        }
    }
}
