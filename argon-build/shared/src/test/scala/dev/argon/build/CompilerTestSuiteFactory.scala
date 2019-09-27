package dev.argon.build

import dev.argon.build.testrunner._
import dev.argon.io.Path
import zio.UIO
import zio.test.Assertion.equalTo
import zio.test.mock.MockEnvironment
import zio.test._
import cats._
import cats.implicits._
import zio.interop.catz._

object CompilerTestSuiteFactory {

  private val libraries = Vector(
    "Argon.Core",
  )

  val referencePaths: UIO[Vector[Path]] = libraries.traverse { name => Path.of(s"libraries/$name/$name.armodule") }

  val testCases: TestCaseStructure = TestCaseLoader.findTestCases(TestCases.all)
  val runners: Seq[TestCaseRunner] = PlatformHelpers.testCaseRunners(referencePaths)

  def createTest(runner: TestCaseRunner)(testCase: TestCase): ZSpec[MockEnvironment, Throwable, String, Unit] =
    testM[MockEnvironment, Throwable, String](testCase.name) {
      assertM(
        runner.runTest(testCase)
          .provideSome(PlatformHelpers.ioEnvironment)
          .orDie,
        equalTo(TestCaseResult.Success : TestCaseResult)
      )
    }

  def createSuites(runner: TestCaseRunner, structure: TestCaseStructure): Seq[ZSpec[MockEnvironment, Throwable, String, Any]] =
    structure.nestedStructures.map { case (name, nested) =>
      suite(name)(createSuites(runner, nested): _*)
    } ++
      structure.tests.map(createTest(runner))

}
