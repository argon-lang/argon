package dev.argon.build

import cats._
import cats.implicits._
import dev.argon.build.testrunner._
import dev.argon.io.Path
import zio._
import zio.interop.catz._
import zio.test._
import zio.test.Assertion._
import zio.test.mock._

object CompilerTests extends DefaultRunnableSpec({

  val libraries = Vector(
    "Argon.Core",
  )

  val referencePaths = libraries.traverse { name => Path.of(s"libraries/$name/$name.armodule") }

  val testCases = TestCaseLoader.findTestCases(TestCases.all)
  val runners = PlatformHelpers.testCaseRunners(referencePaths)

  def createSuites(runner: TestCaseRunner, structure: TestCaseStructure): Seq[ZSpec[MockEnvironment, Throwable, String, Any]] =
    structure.nestedStructures.map { case (name, nested) =>
      suite(name)(createSuites(runner, nested): _*)
    } ++
      structure.tests.map { testCase =>
        testM[MockEnvironment, String, Any](testCase.name) {
          assertM(
            runner.runTest(testCase)
              .provideSome(PlatformHelpers.ioEnvironment)
              .orDie,
            equalTo(TestCaseResult.Success : TestCaseResult)
          )
        }
      }

  suite("Compiler Tests")(
    runners.map { runner =>
      suite(runner.name)(
        createSuites(runner, testCases): _*
      )
    }: _*
  )

})
