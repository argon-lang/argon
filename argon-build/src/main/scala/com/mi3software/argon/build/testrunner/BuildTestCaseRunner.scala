package com.mi3software.argon.build.testrunner


import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{CatsInstances, FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.build._
import com.mi3software.argon.compiler.core.ModuleDescriptor
import com.mi3software.argon.parser.SyntaxError
import fs2._
import com.mi3software.argon.parser.impl.ParseHandler
import shims._
import java.io.File
import shims.effect._

import scalaz.effect.IO

final class BuildTestCaseRunner(backend: Backend, references: Vector[File]) extends TestCaseRunner {
  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    testCase
      .sourceCode
      .zipWithIndex
      .traverse {
        case (InputSourceData(filename, data), i) =>
          val fileSpec = FileSpec(FileID(i), filename)

          Stream(data: _*)
            .covary[EitherT[IO, NonEmptyList[SyntaxError], ?]]
            .through(ParseHandler.parse(fileSpec))
            .translate(ParseHandler.convertSyntaxErrorToCompilationError(fileSpec))
            .compile
            .toVector(CatsInstances.scalazEitherTSync)
      }
      .leftMap { _.map[CompilationError](CompilationError.SyntaxCompilerError) }
      .flatMap { sourceASTs =>

        val input = CompilerInput(
          source = sourceASTs.flatten,
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

  private def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, INil()) =>
        error.getClass.getName === CompilationError.getClass.getName + errorName

      case _ => false
    }
}
