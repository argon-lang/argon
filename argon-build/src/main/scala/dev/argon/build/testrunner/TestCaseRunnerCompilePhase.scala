package dev.argon.build.testrunner

import java.io.File

import dev.argon.build.{Backend, BuildProcess}
import dev.argon.compiler.{CompilationError, CompilerInput, CompilerOptions, IOCompilation}
import dev.argon.compiler.core.ModuleDescriptor
import scalaz._
import Scalaz._
import dev.argon.build.project.{ProjectFileHandler, ProjectLoader}
import scalaz.zio.IO
import dev.argon.util.FileOperations.fileShow

private[testrunner] trait TestCaseRunnerCompilePhase extends TestCaseRunnerParsePhase {

  protected val moduleName = "TestProgram"

  protected val backend: Backend

  protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[Throwable, backend.BackendOptions[Id, File]]

  protected def getProgramOutput(compOutput: backend.TCompilationOutput[IO[Throwable, +?], File]): IO[Throwable, String]

  protected def normalizeOutput(output: String): String =
    output.split("\n").map { _.trim }.filter { _.nonEmpty }.mkString("\n")


  def compileTestCase[A](testCase: TestCase, references: Vector[File]): IO[Throwable, TestCaseResult] =
    IOCompilation.compilationInstance.flatMap { implicit ioComp =>
      ioComp.getResult(
        parseTestCaseSource(testCase)
          .flatMap { parsedSource =>
            val compilerOptions = CompilerOptions[Id](
              moduleName = moduleName
            )

            backendOptions(compilerOptions).flatMap { backendOpts =>
              BuildProcess.compile(
                backend
              )(
                parsedSource,
                references,
                compilerOptions,
                backendOpts,
              )(getProgramOutput)(implicitly, ioComp, IOCompilation.fileSystemResourceAccess)
            }
          }
      )
        .map {
          case (_, Right(programOutput)) =>
            testCase.expectedResult match {
              case TestCaseExpectedOutput(expectedOutput) if normalizeOutput(programOutput) === normalizeOutput(expectedOutput) =>
                TestCaseResult.Success

              case _ =>
                TestCaseResult.Failure(
                  TestCaseActualResult.Output(programOutput),
                  testCase.expectedResult
                )
            }

          case (_, Left(errors)) =>
            testCase.expectedResult match {
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

            }
        }
    }

}
