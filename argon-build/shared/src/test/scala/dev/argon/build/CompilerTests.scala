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
import zio.interop.catz._
import zio.test._
import zio.test.Assertion._
import zio.test.mock._
import PlatformHelpers.TestExecEnv

object CompilerTests extends CompilerTestSuiteBase {
  override val suiteName: String = "Compiler Tests"

  override val testCases: ZIO[FileIO with System, TestFailure[Failure], TestCaseStructure] =
    ZIO.accessM[System](_.get.env("ARGON_TEST_CASES").orDie)
      .get
      .mapError { _ => IO.fail(new Exception("ARGON_TEST_CASES was unset")) }
      .flatMap(Path.of(_))
      .flatMap(TestCaseLoader.loadTestCases _)
      .catchAllCause { cause => IO.fail(TestFailure.halt[Failure](cause)) }


  override protected def runners(references: Vector[TestResourceIndicator]): Seq[TestCaseRunner[TestExecEnv]] =
    PlatformHelpers.testCaseRunners(references)
}
