package dev.argon.build

import java.io.IOException

import dev.argon.build.testrunner._
import dev.argon.io.Path
import zio._
import zio.system.System
import zio.test.Assertion.equalTo
import zio.test._
import cats._
import cats.implicits._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import zio.interop.catz._
import PlatformHelpers.TestExecEnv

abstract class CompilerTestSuiteBase extends DefaultRunnableSpec {

  private val libraries = Vector(
    "Argon.Core",
  )

  private val references: Vector[TestResourceIndicator] =
    libraries.map(LibraryResourceIndicator.apply)

  protected val suiteName: String
  protected def testCases: ZIO[FileIO with System, TestFailure[Failure], TestCaseStructure]
  protected def runners(references: Vector[TestResourceIndicator]): Seq[TestCaseRunner[TestExecEnv]]

  private def isExpectedResult(runner: TestCaseRunner[_])(expected: TestCaseExpectedResult): Assertion[TestCaseActualResult] =
    Assertion.assertion("isExpectedResult")(Assertion.Render.param(expected)) { actual =>
      runner.isResultExpected(actual, expected)
    }

  private def createTest(runner: TestCaseRunner[TestExecEnv])(testCase: TestCase): ZSpec[PlatformHelpers.BaseLayer, Failure] =
    testM(testCase.name) {
      runner.runTest(testCase)
        .provideLayer(PlatformHelpers.execEnvLayer)
        .orDie
        .map { result => assert(result)(isExpectedResult(runner)(testCase.expectedResult)) }
    }

  private def createSuites(runner: TestCaseRunner[TestExecEnv], structure: TestCaseStructure): Seq[ZSpec[PlatformHelpers.BaseLayer, Failure]] =
    structure.nestedStructures.map { case (name, nested) =>
      suite(name)(createSuites(runner, nested): _*)
    } ++
      structure.tests.map(createTest(runner))

  override def spec: ZSpec[Environment, Failure] =
    Spec.suite(suiteName,
      for {
        loadedTestCases <- testCases
      } yield runners(references).map { runner =>
        suite(runner.name)(
          createSuites(runner, loadedTestCases): _*
        )
      }.toVector,
      None
    ).provideSomeLayer[Environment](PlatformHelpers.baseLayer)
}
