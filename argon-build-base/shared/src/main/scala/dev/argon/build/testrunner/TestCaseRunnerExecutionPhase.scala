package dev.argon.build.testrunner

import dev.argon.build.BuildResult
import dev.argon.compiler.CompilationError
import dev.argon.options.FileList
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.util.MaybeBlocking
import zio._

private[testrunner] abstract class TestCaseRunnerExecutionPhase[-R <: FileIO with ZipRead with MaybeBlocking] extends TestCaseRunnerCompilePhase[R] {

  protected val references: FileList

  override def runTest(testCase: TestCase): ZIO[R, CompilationError, TestCaseCompletedResult] =
    compileTestCase(testCase, references)
      .use(getProgramOutput)
      .map(TestCaseActualResult.Output)

  protected def getProgramOutput(compOutput: BuildResult.Aux[backend.type]): ZIO[R, CompilationError, String]

}
