package dev.argon.build


import dev.argon.build.testrunner._
import dev.argon.io.fileio.FileIO
import zio._
import zio.test._
import zio.test.environment._

object CompilerTests extends CompilerTestSuiteBase {
  override val suiteName: String = "Compiler Tests"


  override protected def testCases: ZIO[FileIO with Live, TestFailure[Failure], TestCaseStructure] =
    live(system.env("ARGON_TEST_CASES").orDie)
      .get
      .mapError { _ => IO.fail(new RuntimeException("ARGON_TEST_CASES was unset")) }
      .flatMap(TestCaseLoader.loadTestCases(_))
      .catchAllCause { cause => IO.fail(TestFailure.halt[Failure](cause)) }

}
