package com.mi3software.argon.build.testrunner


import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.build._
import com.mi3software.argon.compiler.core.ModuleDescriptor
import java.io.File

import scalaz.effect.IO

final class BuildTestCaseRunner(backend: Backend, references: Vector[File]) extends TestCaseRunner with TestCaseRunnerCompilePhase {
  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    compileTestCase(testCase, backend, references)
      .map { _ => TestCaseResult.Success }
      .run
      .map { _.merge }
}
