package dev.argon.build

import java.io.IOException

import cats._
import cats.implicits._
import dev.argon.build.testrunner._
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._
import zio.interop.catz._
import zio.test._
import zio.test.Assertion._
import zio.test.mock._

object CompilerTests extends CompilerTestSuiteBase {
  override val suiteName: String = "Compiler Tests"

  override val testCases: ZIO[Environment, TestFailure[Failure], TestCaseStructure] =
    ZIO.accessM[FileIO](_.get.getEnv("ARGON_TEST_CASES"))
      .flatMap {
        case Some(value) => IO.succeed(value)
        case None => IO.fail(new Exception("ARGON_TEST_CASES was unset"))
      }
      .flatMap(Path.of(_))
      .flatMap(TestCaseLoader.loadTestCases _)
      .provideLayer(PlatformHelpers.fileIOLayer)
      .catchAllCause { cause => IO.fail(TestFailure.halt[Failure](cause)) }


  override val runners: Seq[TestCaseRunner] = PlatformHelpers.testCaseRunners(referencePaths)
}
