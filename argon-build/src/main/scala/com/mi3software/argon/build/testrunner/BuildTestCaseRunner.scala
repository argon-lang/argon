package com.mi3software.argon.build.testrunner

import scalaz._
import Scalaz._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.build._
import java.io.File

import scalaz.zio.IO

final class BuildTestCaseRunner(backend: Backend, references: Vector[File]) extends TestCaseRunner with TestCaseRunnerCompilePhase {
  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(testCase, backend, references)
      .map { _ => TestCaseResult.Success }
      .run
      .map { _.merge }
}
