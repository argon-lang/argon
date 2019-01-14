package com.mi3software.argon.build.testrunner

import java.io.File

import com.mi3software.argon.build.{Backend, BuildProcess, CompilationOutput, CompilationResult}
import com.mi3software.argon.compiler.{CompilationError, CompilerInput, CompilerOptions}
import com.mi3software.argon.compiler.core.ModuleDescriptor
import scalaz._
import scalaz.effect.IO

private[testrunner] trait TestCaseRunnerCompilePhase extends TestCaseRunnerParsePhase {

  protected val moduleDescriptor = ModuleDescriptor("TestProgram")

  def compileTestCase(testCase: TestCase, backend: Backend, references: Vector[File]): EitherT[IO, TestCaseResult, (CompilationOutput, String)] =
    EitherT(
      BuildProcess.compile(
        backend,
        parseTestCaseSource(testCase),
        references,
        CompilerOptions(
          moduleDescriptor = moduleDescriptor
        ),
      )
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
    )

}
