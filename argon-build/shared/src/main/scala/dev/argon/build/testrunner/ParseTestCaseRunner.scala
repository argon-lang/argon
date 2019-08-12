package dev.argon.build.testrunner

import dev.argon.compiler.IOCompilation
import dev.argon.io.FileIO
import zio.blocking.Blocking
import zio.{IO, Runtime, ZIO}
import dev.argon.build._

object ParseTestCaseRunner extends TestCaseRunner with TestCaseRunnerParsePhase {

  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    IOCompilation.compilationInstance[BuildEnvironment].flatMap { implicit ioComp =>
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
