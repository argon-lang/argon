package com.mi3software.argon.build.testrunner


import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{CatsInstances, FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.build._
import com.mi3software.argon.compiler.core.ModuleDescriptor
import java.io.File

import scalaz.effect.IO

final class BuildTestCaseRunner(backend: Backend, references: Vector[File]) extends TestCaseRunner with TestCaseRunnerParsePhase {
  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    parseTestCaseSource(testCase)
      .flatMap { sourceASTs =>

        val input = CompilerInput(
          source = sourceASTs,
          references = references,
          options = CompilerOptions(
            moduleDescriptor = ModuleDescriptor("TestProgram")
          )
        )

        EitherT[IO, NonEmptyList[CompilationError], CompilationOutput](backend.compile(input).map { _.result })
      }
      .run
      .map {
        case \/-(_) =>
          testCase.expectedResult match {
            case TestCaseExpectedOutput(_) => TestCaseResult.Success
            case TestCaseExpectedError(_) =>
              TestCaseResult.Failure(
                TestCaseActualResult.Output(""),
                testCase.expectedResult
              )

          }

        case -\/(errors) =>
          testCase.expectedResult match {
            case TestCaseExpectedOutput(_) =>
              TestCaseResult.Failure(
                TestCaseActualResult.Errors(errors),
                TestCaseExpectedOutput("")
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
