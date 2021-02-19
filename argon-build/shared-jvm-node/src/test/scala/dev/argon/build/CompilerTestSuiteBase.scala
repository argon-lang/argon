package dev.argon.build

import java.io.IOException
import java.time.Duration

import dev.argon.build.testrunner._
import zio._
import zio.system.System
import zio.test.Assertion.equalTo
import zio.test._
import cats._
import cats.implicits._
import dev.argon.options.FileList
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.platform._
import zio.interop.catz.core._
import dev.argon.io.FileNameUtil
import dev.argon.util.MaybeBlocking
import zio.test.environment._

abstract class CompilerTestSuiteBase extends PlatformRunnableSpec {

  private type TestExecEnv =
    FileIO with ZipRead with MaybeBlocking

  private val libraries = Vector(
    "Argon.Core",
  )

  private val references: ZIO[Live, Nothing, FileList] = {
    live(ZIO.accessM[System](_.get.env("ARGON_LIB_DIR"))).orDie.flatMap {
      case None => IO.die(new RuntimeException("Library directory was unspecified"))
      case Some(libDir) =>
        IO.succeed(new FileList(libraries.map { lib => FileNameUtil.combine(libDir, FileNameUtil.combine(lib, FileNameUtil.combine("bin", lib + ".armodule"))) }.toList))
    }
  }

  protected val suiteName: String
  protected def testCases: ZIO[FileIO with Live, TestFailure[Failure], TestCaseStructure]

  private def execEnvLayer: ZLayer[Environment, Throwable, TestExecEnv] =
    FileIOPlatform.live ++ ZipReadPlatform.live ++ ZLayer.requires[MaybeBlocking]

  private def isExpectedResult(runner: TestCaseRunner[_])(expected: TestCaseExpectedResult): Assertion[TestCaseActualResult] =
    Assertion.assertion("isExpectedResult")(Assertion.Render.param(expected)) { actual =>
      runner.isResultExpected(actual, expected)
    }

  private def createTest(runner: TestCaseRunner[TestExecEnv])(testCase: TestCase): ZSpec[Environment, Failure] = {
    val testInfo =
      testM(testCase.name) {
        runner.runTest(testCase)
          .catchAllCause(cause => IO.succeed(TestCaseActualResult.Errors(cause)))
          .provideLayer(execEnvLayer)
          .orDie
          .map { result => assert(result)(isExpectedResult(runner)(testCase.expectedResult)) }
      } @@ TestAspect.diagnose(Duration.ofSeconds(10))

    if(testCase.enabled)
      testInfo
    else
      testInfo @@ TestAspect.ignore
  }

  private def createSuites(runner: TestCaseRunner[TestExecEnv], structure: TestCaseStructure): Seq[ZSpec[Environment, Failure]] =
    structure.nestedStructures.map { case (name, nested) =>
      suite(name)(createSuites(runner, nested): _*)
    } ++
      structure.tests.map(createTest(runner))

  private def allRunners: URIO[BackendProvider with Live with TestExecEnv, Seq[TestCaseRunner[TestExecEnv]]] =
    references.flatMap { refs =>
      ZIO.access { env =>
        env.get[BackendProvider.Service].testCaseRunners(refs)
      }
    }


  override def spec: ZSpec[Environment, Failure] =
    Spec.suite(suiteName,
      ZManaged.fromEffect(
        for {
          loadedTestCases <- testCases
          runners <- allRunners.provideSomeLayer[Environment](execEnvLayer.orDie ++ BackendProviderImpl.live)
        } yield runners.map { runner =>
          suite(runner.name)(
            createSuites(runner, loadedTestCases): _*
          )
        }.toVector
      ),
      None
    )
}
