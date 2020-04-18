package dev.argon.build

import dev.argon.build.testrunner._
import dev.argon.io.Path
import zio.UIO
import zio.test.Assertion.equalTo
import zio.test._
import cats._
import cats.implicits._
import zio.interop.catz._

abstract class CompilerTestSuiteBase extends DefaultRunnableSpec {

  private val libraries = Vector(
    "Argon.Core",
  )

  val referencePaths: UIO[Vector[Path]] = libraries.traverse { name => Path.of(s"libraries/$name/$name.armodule") }

  val suiteName: String
  val testCases: TestCaseStructure
  val runners: Seq[TestCaseRunner]

  def createTest(runner: TestCaseRunner)(testCase: TestCase): ZSpec[Environment, Failure] =
    testM(testCase.name) {
      assertM(
        runner.runTest(testCase)
          .provideCustomLayer(PlatformHelpers.fileIOLayer)
          .orDie
      )(
        equalTo(TestCaseResult.Success : TestCaseResult)
      )
    }

  def createSuites(runner: TestCaseRunner, structure: TestCaseStructure): Seq[ZSpec[Environment, Failure]] =
    structure.nestedStructures.map { case (name, nested) =>
      suite(name)(createSuites(runner, nested): _*)
    } ++
      structure.tests.map(createTest(runner))

  override def spec: ZSpec[Environment, Failure] =
    suite(suiteName)(
      runners.map { runner =>
        suite(runner.name)(
          createSuites(runner, testCases): _*
        )
      }: _*
    )
}
