package dev.argon.build.testrunner

import dev.argon.backend.ResourceAccess
import dev.argon.compiler.loaders.ResourceIndicator
import zio.{IO, URIO, ZIO}

trait TestCaseRunnerExecutionPhase[-R <: ResourceAccess] extends TestCaseRunnerCompilePhase[R] {

  protected val references: Vector[ResourceIndicator]

  override def runTest(testCase: TestCase): URIO[R, TestCaseActualResult] =
    compileTestCase(testCase, references)
      .use(getProgramOutput)
      .map(TestCaseActualResult.Output)
      .catchAll(IO.succeed(_))

  protected def getProgramOutput(compOutput: backend.TCompilationOutput): ZIO[R, TestCaseError, String]

  protected final def executionFailureResult(error: Throwable): TestCaseError =
    TestCaseActualResult.ExecutionError(error)

}
