package dev.argon.build

import cats._
import cats.implicits._
import dev.argon.build.testrunner._
import dev.argon.io.Path
import org.scalatest.{BeforeAndAfterAll, FunSpec, Matchers}
import zio._
import zio.interop.catz._

class CompilerTests extends FunSpec with DefaultRuntime with Matchers with BeforeAndAfterAll {

  private def unsafeRunBuild[E, A](action: ZIO[BuildEnvironment, E, A]): A =
    unsafeRun(
      action.provideSome(PlatformHelpers.ioEnvironment)
    )

  private def processTestCases(runner: TestCaseRunner, structure: TestCaseStructure): Unit = {

    for((name, nestedStructures) <- structure.nestedStructures) {
      describe(name) {
        processTestCases(runner, nestedStructures)
      }
    }

    for(testCase <- structure.tests) {
      it(testCase.name) {
        unsafeRunBuild(runner.runTest(testCase)) shouldBe TestCaseResult.Success
      }
    }

  }

  private val libraries = Vector(
    "Argon.Core",
  )

  private val references = unsafeRun(libraries.traverse { name => Path.of(s"libraries/$name/$name.armodule") })

  private def generateTestCases(): Unit = {
    val testCases = TestCaseLoader.findTestCases(TestCases.all)

    val runners = PlatformHelpers.testCaseRunners(references)

    runners foreach { runner =>
      describe(runner.name) {
        processTestCases(runner, testCases)
      }
    }
  }

  generateTestCases()
}
