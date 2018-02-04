package com.mi3software.argon.compiler

import java.io.File

import com.mi3software.argon.testrunner.{TestCaseLoader, TestCaseResult, TestCaseRunner, TestCaseStructure}
import org.scalatest.{FunSpec, Matchers}

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
        runner.parse(testCase) shouldBe TestCaseResult.Success
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
