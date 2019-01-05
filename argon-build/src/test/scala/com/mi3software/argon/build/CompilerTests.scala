package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.build.testrunner._
import org.scalatest.{FunSpec, Matchers}
import scalaz.effect.IO
import shims.effect._

class CompilerTests extends FunSpec with Matchers {

  private def processTestCases(runner: TestCaseRunner, structure: TestCaseStructure): Unit = {

    for((name, nestedStructures) <- structure.nestedStructures) {
      describe(name) {
        processTestCases(runner, nestedStructures)
      }
    }

    for(testCase <- structure.tests) {
      it(testCase.name) {
        runner.runTest[IO](testCase).unsafePerformIO() shouldBe TestCaseResult.Success
      }
    }

  }


  private def generateTestCases(): Unit = {
    val testCases = TestCaseLoader.findTestCases(new File(getClass.getResource("/com/mi3software/argon/compiler/testcases/").toURI)).unsafePerformIO()

    val runners = Vector(
      "Parsing" -> ParseTestCaseRunner,
    )

    runners foreach { case (desc, runner) =>
      describe(desc) {
        processTestCases(runner, testCases)
      }
    }
  }

  generateTestCases()
}
