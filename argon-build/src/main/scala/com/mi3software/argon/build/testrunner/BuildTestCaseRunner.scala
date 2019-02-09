package com.mi3software.argon.build.testrunner

import scalaz._
import Scalaz._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.build._
import java.io.File

import scalaz.zio.IO

final class BuildTestCaseRunner(protected val backend: Backend, references: Vector[File]) extends TestCaseRunnerCompilePhase {


  override protected def getProgramOutput(compOutput: backend.TCompilationOutput[IO[Throwable, +?]]): IO[Throwable, String] =
    IO.now("")

  override protected def normalizeOutput(output: String): String = ""

  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(testCase, references)
}
