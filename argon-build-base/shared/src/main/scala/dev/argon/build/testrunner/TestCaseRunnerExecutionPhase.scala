package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import zio._

private[testrunner] abstract class TestCaseRunnerExecutionPhase[I <: ResourceIndicator: Tag, -R <: ResourceReader[I]] extends TestCaseRunnerCompilePhase[I, R] {

  protected val references: Vector[I]

  override def runTest(testCase: TestCase): ZIO[R, CompilationError, TestCaseCompletedResult] =
    compileTestCase(testCase, references)
      .use(getProgramOutput)
      .map(TestCaseActualResult.Output)

  protected def getProgramOutput(compOutput: backend.TCompilationOutput): ZIO[R, CompilationError, String]

}
