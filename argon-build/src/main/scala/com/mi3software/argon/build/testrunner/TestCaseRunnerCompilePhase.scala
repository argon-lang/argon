package com.mi3software.argon.build.testrunner

import java.io.File

import com.mi3software.argon.build.{Backend, BuildProcess}
import com.mi3software.argon.compiler.{CompilationError, CompilerInput, CompilerOptions, IOCompilation}
import com.mi3software.argon.compiler.core.ModuleDescriptor
import scalaz._
import Scalaz._
import com.mi3software.argon.build.project.{ProjectFileHandler, ProjectLoader}
import scalaz.zio.IO
import com.mi3software.argon.util.FileOperations.fileShow

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
              )(implicitly, ioComp, IOCompilation.fileSystemResourceAccess)
            }
          }
          .flatMap(getProgramOutput)
      )
        .map {
          case (_, \/-(programOutput)) =>
            testCase.expectedResult match {
              case TestCaseExpectedOutput(expectedOutput) if normalizeOutput(programOutput) === normalizeOutput(expectedOutput) =>
                TestCaseResult.Success

              case _ =>
                TestCaseResult.Failure(
                  TestCaseActualResult.Output(programOutput),
                  testCase.expectedResult
                )
            }

          case (_, -\/(errors)) =>
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
