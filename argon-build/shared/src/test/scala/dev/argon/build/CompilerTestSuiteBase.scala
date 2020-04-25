package dev.argon.build

import java.io.IOException

import dev.argon.build.testrunner._
import dev.argon.io.Path
import zio.{IO, RIO, UIO, ZIO}
import zio.test.Assertion.equalTo
import zio.test._
import cats._
import cats.implicits._
import dev.argon.io.fileio.FileIO
import zio.interop.catz._

abstract class CompilerTestSuiteBase extends DefaultRunnableSpec {

  private val libraries = Vector(
    "Argon.Core",
  )

  val referencePaths: RIO[FileIO, Vector[Path]] = libraries.traverse { name =>
    ZIO.accessM[FileIO](_.get.getEnv("ARGON_LIB_DIR"))
      .flatMap {
        case Some(libDir) => Path.of(libDir,name, name + ".armodule")
        case None => IO.fail(new RuntimeException("ARGON_LIB_DIR was not set"))
      }
  }

  val suiteName: String
  def testCases: ZIO[Environment, TestFailure[Failure], TestCaseStructure]
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

    Spec.suite(suiteName,
      testCases
        .map { loadedTestCases =>
          runners.map { runner =>
            suite(runner.name)(
              createSuites(runner, loadedTestCases): _*
            )
          }.toVector
        },
      None
    )
}
