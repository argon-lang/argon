package dev.argon.build.testrunner

import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import zio._

private[testrunner] abstract class TestCaseRunnerExecutionPhase[I <: ResourceIndicator: Tag, -R <: ResourceReader[I]] extends TestCaseRunnerCompilePhase[I, R] {

  protected val references: Vector[I]

  override def runTest(testCase: TestCase): URIO[R, TestCaseActualResult] =
    compileTestCase(testCase, references)
      .use(getProgramOutput)
      .map(TestCaseActualResult.Output)
      .catchAll(IO.succeed(_))

  protected def getProgramOutput(compOutput: backend.TCompilationOutput): ZIO[R, TestCaseError, String]

  protected final def executionFailureResult(error: Throwable): TestCaseError =
    TestCaseActualResult.ExecutionError(error)

}
