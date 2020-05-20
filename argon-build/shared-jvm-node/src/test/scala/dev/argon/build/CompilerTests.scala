package dev.argon.build

import java.io.IOException

import cats._
import cats.implicits._
import dev.argon.build.testrunner._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._
import zio.system.System
import zio.interop.catz.core._
import zio.test._
import zio.test.environment._
import zio.test.Assertion._
import zio.test.environment.Live
import zio.test.mock._

object CompilerTests extends CompilerTestSuiteBase {
  override val suiteName: String = "Compiler Tests"


  override protected def testCases[P: Path : Tag]: ZIO[FileIO[P] with Live, TestFailure[Failure], TestCaseStructure] =
    live(system.env("ARGON_TEST_CASES").orDie)
      .get
      .mapError { _ => IO.fail(new Exception("ARGON_TEST_CASES was unset")) }
      .flatMap(Path.of(_))
      .flatMap(TestCaseLoader.loadTestCases(_))
      .catchAllCause { cause => IO.fail(TestFailure.halt[Failure](cause)) }

}
