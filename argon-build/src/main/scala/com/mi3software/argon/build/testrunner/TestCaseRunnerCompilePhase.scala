package com.mi3software.argon.build.testrunner

import java.io.File

import com.mi3software.argon.build.{Backend, BuildProcess, CompilationOutput, CompilationResult}
import com.mi3software.argon.compiler.{CompilationError, CompilerInput, CompilerOptions, IOCompilation}
import com.mi3software.argon.compiler.core.ModuleDescriptor
import scalaz._
import scalaz.zio.IO
import com.mi3software.argon.util.FileOperations.fileShow

private[testrunner] trait TestCaseRunnerCompilePhase extends TestCaseRunnerParsePhase {

  protected val moduleDescriptor = ModuleDescriptor("TestProgram")

  def compileTestCase(testCase: TestCase, backend: Backend, references: Vector[File]): EitherT[IO[Throwable, ?], TestCaseResult, (backend.TCompilationOutput, String)] =
    EitherT(
      IOCompilation.compilationInstance.flatMap { implicit ioComp =>
        parseTestCaseSource(testCase)
          .flatMap { parsedSource =>
            BuildProcess.compile(
              backend,
              parsedSource,
              references,
              CompilerOptions(
                moduleDescriptor = moduleDescriptor
              ),
            )(ioComp, implicitly, ioComp, IOCompilation.fileSystemResourceAccess)
          }
          .map { _.result match {
            case \/-(compilationOutput) =>
              testCase.expectedResult match {
                case TestCaseExpectedOutput(expectedOutput) =>
                  \/-((compilationOutput, expectedOutput))

                case TestCaseExpectedError(_) =>
                  -\/(TestCaseResult.Failure(
                    TestCaseActualResult.Output(""),
                    testCase.expectedResult
                  ))

              }

            case -\/(errors) =>
              -\/(testCase.expectedResult match {
                case TestCaseExpectedOutput(_) =>
                  TestCaseResult.Failure(
                    TestCaseActualResult.Errors(errors),
                    testCase.expectedResult
                  )

                case TestCaseExpectedError(errorName) if isExpectedError(errors, errorName) =>
                  TestCaseResult.Success

                case TestCaseExpectedError(_) =>
                  TestCaseResult.Failure(
                    TestCaseActualResult.Errors(errors),
                    testCase.expectedResult
                  )

              })
          } }
      }
    )

}
