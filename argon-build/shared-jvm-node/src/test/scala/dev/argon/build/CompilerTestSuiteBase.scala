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
import dev.argon.backend.{ResourceReader, ResourceWriter}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.platform._
import zio.interop.catz.core._
import zio.test.environment.Live

abstract class CompilerTestSuiteBase extends PlatformRunnableSpec {

  private type TestExecEnv =
    ResourceReader[TestResourceIndicator] with
      Has[TestResourceReader.Service[FilePath]] with
      ResourceWriter[Nothing] with
      FileIO[FilePath]

  private val libraries = Vector(
    "Argon.Core",
  )

  private val references: Vector[TestResourceIndicator] =
    libraries.map(LibraryResourceIndicator.apply)

  protected val suiteName: String
  protected def testCases[P : Path : Tagged]: ZIO[FileIO[P] with Live, TestFailure[Failure], TestCaseStructure]

  private def execEnvLayer: ZLayer[Environment, Throwable, TestExecEnv] =
    TestResourceReader.layer[FilePath].passthrough >>> (
      ZLayer.identity[FileIO[FilePath] with Has[TestResourceReader.Service[FilePath]]] ++
        ZLayer.fromFunction[Has[TestResourceReader.Service[FilePath]], ResourceReader.Service[TestResourceIndicator]](_.get) ++
        ResourceWriter.forNothing
      )

  private def isExpectedResult(runner: TestCaseRunner[_])(expected: TestCaseExpectedResult): Assertion[TestCaseActualResult] =
    Assertion.assertion("isExpectedResult")(Assertion.Render.param(expected)) { actual =>
      runner.isResultExpected(actual, expected)
    }

  private def createTest(runner: TestCaseRunner[TestExecEnv])(testCase: TestCase): ZSpec[Environment, Failure] =
    testM(testCase.name) {
      runner.runTest(testCase)
        .provideLayer(execEnvLayer)
        .orDie
        .map { result => assert(result)(isExpectedResult(runner)(testCase.expectedResult)) }
    }

  private def createSuites(runner: TestCaseRunner[TestExecEnv], structure: TestCaseStructure): Seq[ZSpec[Environment, Failure]] =
    structure.nestedStructures.map { case (name, nested) =>
      suite(name)(createSuites(runner, nested): _*)
    } ++
      structure.tests.map(createTest(runner))

  private def allRunners: URIO[BackendProvider with Has[TestResourceReader.Service[FilePath]], Seq[TestCaseRunner[ResourceReader[TestResourceIndicator] with ResourceWriter[Nothing] with FileIO[FilePath]]]] =
    ZIO.access { env =>
      val testResReader = env.get[TestResourceReader.Service[FilePath]]

      def pathResolver(id: TestResourceIndicator): UIO[FilePath] = id match {
        case LibraryResourceIndicator(name) =>
          testResReader.getLibPath(name)
      }

      env.get[BackendProvider.Service].testCaseRunners(references, pathResolver)
    }

  override def spec: ZSpec[Environment, Failure] =
    Spec.suite(suiteName,
      for {
        loadedTestCases <- testCases
        runners <- allRunners.provideSomeLayer[Environment](execEnvLayer.orDie ++ BackendProviderImpl.live)
      } yield runners.map { runner =>
        suite(runner.name)(
          createSuites(runner, loadedTestCases): _*
        )
      }.toVector,
      None
    )
}
