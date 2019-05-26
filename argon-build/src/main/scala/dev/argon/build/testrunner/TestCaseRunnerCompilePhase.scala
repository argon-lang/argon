package dev.argon.build.testrunner

import java.io.{File, IOException}

import dev.argon.build.{Backend, BuildProcess}
import dev.argon.compiler._
import dev.argon.compiler.core.ModuleDescriptor
import cats._
import cats.implicits._
import dev.argon.build.project.{ProjectFileHandler, ProjectLoader}
import scalaz.zio._
import dev.argon.util.FileOperations.fileShow
import IOCompilation.fileSystemResourceAccess
import cats.data.NonEmptyList
import scalaz.zio.blocking.Blocking

private[testrunner] trait TestCaseRunnerCompilePhase extends TestCaseRunnerParsePhase {

  protected val moduleName = "TestProgram"

  protected val backend: Backend

  protected def backendOptions(compilerOptions: CompilerOptions[Id]): ZIO[Blocking, IOException, backend.BackendOptions[Id, File]]

  protected def getProgramOutput(compOutput: backend.TCompilationOutput[ZIO, Blocking, File]): ZIO[Blocking, NonEmptyList[CompilationError], Either[Throwable, String]]

  protected def normalizeOutput(output: String): String =
    output.split("\n").map { _.trim }.filter { _.nonEmpty }.mkString("\n")


  def compileTestCase(testCase: TestCase, references: Vector[File]): ZIO[Blocking, Nothing, TestCaseResult] =
    IOCompilation.compilationInstance[Blocking].flatMap { implicit ioComp =>

      val result: ZIO[Blocking, Nothing, (Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], Either[Throwable, String]])] =
        ioComp.getResult(
          parseTestCaseSource(testCase)
            .flatMap { parsedSource =>
              val compilerOptions = CompilerOptions[Id](
                moduleName = moduleName
              )

              backendOptions(compilerOptions)
                .either
                .flatMap {
                  case Left(ex) => IO.succeed(Left(ex))
                  case Right(backendOpts) =>
                    BuildProcess.compile(
                      backend
                    )(
                      parsedSource,
                      references,
                      compilerOptions,
                      backendOpts,
                    )(getProgramOutput)
                }
            }
        )

      result
        .map {
          case (_, Right(Left(ex))) => TestCaseResult.ExecutionError(ex)

          case (_, Right(Right(programOutput))) =>
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
