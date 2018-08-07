package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.build.testrunner.{TestCaseLoader, TestCaseResult, TestCaseRunner, TestCaseStructure}
import org.scalatest.{FunSpec, Matchers}
import scalaz.effect.IO
import shims.effect._

class CompilerTests extends FunSpec with Matchers {

  private val runner = new TestCaseRunner

  private def processTestCases(structure: TestCaseStructure): Unit = {

    for((name, nestedStructures) <- structure.nestedStructures) {
      describe(name) {
        processTestCases(nestedStructures)
      }
    }

    for(testCase <- structure.tests) {
      it(testCase.name) {
        runner.parse[IO](testCase).unsafePerformIO() shouldBe TestCaseResult.Success
      }
    }

  }


  private def generateTestCases(): Unit = {
    val testCases = TestCaseLoader.findTestCases(new File(getClass.getResource("/com/mi3software/argon/compiler/testcases/").toURI)).unsafePerformIO()
    describe(s"Parsing") {
      processTestCases(testCases)
    }
  }

  generateTestCases()
}
