package dev.argon.build.testrunner

import java.io.{File, IOException}

import dev.argon.build.BuildProcess
import dev.argon.compiler._
import dev.argon.compiler.core.ModuleDescriptor
import cats._
import cats.implicits._
import dev.argon.compiler.backend.ProjectFileHandler
import zio._
import dev.argon.io.FileOperations.fileShow
import cats.data.NonEmptyList
import dev.argon.compiler.backend.Backend
import dev.argon.io.FileIO
import zio.blocking.Blocking
import dev.argon.build._

private[testrunner] trait TestCaseRunnerCompilePhase extends TestCaseRunnerParsePhase {

  protected val moduleName = "TestProgram"

  protected val backend: Backend

  protected def backendOptions(compilerOptions: CompilerOptions[Id]): ZIO[BuildEnvironment, IOException, backend.BackendOptions[Id, File]]

  protected def getProgramOutput(compOutput: backend.TCompilationOutput { val context: Backend.ContextWithComp[ZIO, BuildEnvironment, File] }): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]]

  protected def normalizeOutput(output: String): String =
    output.split("\n").map { _.trim }.filter { _.nonEmpty }.mkString("\n")


  def compileTestCase(testCase: TestCase, references: Vector[File]): ZIO[BuildEnvironment, Nothing, TestCaseResult] =
    IOCompilation.compilationInstance[BuildEnvironment].flatMap { implicit ioComp =>

      val result: ZIO[BuildEnvironment, Nothing, (Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], Either[Throwable, String]])] =
        ZIO.access[FileIO] { env => IOCompilation.fileSystemResourceAccessFactory[BuildEnvironment](env.fileIO) }
          .flatMap { implicit resFactory =>
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
                          backend : backend.type
                        )(
                          parsedSource,
                          references,
                          compilerOptions,
                          backendOpts,
                        )(getProgramOutput)
                    }
                }
            )
          }

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
