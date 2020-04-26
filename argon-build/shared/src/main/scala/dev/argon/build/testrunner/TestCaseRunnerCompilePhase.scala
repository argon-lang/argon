package dev.argon.build.testrunner

import java.io.IOException

import dev.argon.io.Path
import dev.argon.build.BuildProcess
import dev.argon.compiler._
import dev.argon.compiler.core.ModuleDescriptor
import cats._
import cats.implicits._
import zio._
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, ProjectFileHandler, ResourceAccess}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator

private[testrunner] trait TestCaseRunnerCompilePhase extends TestCaseRunnerParsePhase {

  protected val moduleName = "TestProgram"

  protected val backend: Backend

  protected def backendOptions(compilerOptions: CompilerOptions[Id]): ZIO[BuildEnvironment, IOException, backend.BackendOptions[Id, ResourceIndicator]]

  protected def getProgramOutput(compOutput: backend.TCompilationOutput): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]]

  protected def normalizeOutput(output: String): String =
    output.split("\n").map { _.trim }.filter { _.nonEmpty }.mkString("\n")


  def compileTestCase(testCase: TestCase, references: Vector[ResourceIndicator]): ZIO[BuildEnvironment, Nothing, TestCaseResult] =
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
              ).use(getProgramOutput)
          }
      }
      .provideSomeLayer[BuildEnvironment](ResourceAccess.forFileIO)
      .either
      .map {
        case Right(Left(ex)) => TestCaseResult.ExecutionError(ex)

        case Right(Right(programOutput)) =>
          testCase.expectedResult match {
            case TestCaseExpectedOutput(expectedOutput) if normalizeOutput(programOutput) === normalizeOutput(expectedOutput) =>
              TestCaseResult.Success

            case _ =>
              TestCaseResult.Failure(
                TestCaseActualResult.Output(programOutput),
                testCase.expectedResult
              )
          }

        case Left(errors) =>
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
