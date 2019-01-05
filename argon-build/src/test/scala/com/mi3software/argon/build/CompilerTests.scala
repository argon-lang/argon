package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.build.testrunner._
import org.scalatest.{FunSpec, Matchers}

class CompilerTests extends FunSpec with Matchers {

  private def processTestCases(runner: TestCaseRunner, structure: TestCaseStructure): Unit = {

    for((name, nestedStructures) <- structure.nestedStructures) {
      describe(name) {
        processTestCases(runner, nestedStructures)
      }
    }

    for(testCase <- structure.tests) {
      it(testCase.name) {
        runner.runTest(testCase).unsafePerformIO() shouldBe TestCaseResult.Success
      }
    }

  }

  private val libraries = Vector(
    "Argon.Core",
  )

  private val references = libraries.map { name => new File(s"libraries/$name/$name.armodule") }

  private def generateTestCases(): Unit = {
    val testCases = TestCaseLoader.findTestCases(new File(getClass.getResource("/com/mi3software/argon/compiler/testcases/").toURI)).unsafePerformIO()

    val runners = Vector(
      "Parsing" -> ParseTestCaseRunner,
    ) ++ Backend.allBackends.map { backend =>
      s"Compilation (${backend.name})" -> new BuildTestCaseRunner(backend, references)
    }

    runners foreach { case (desc, runner) =>
      describe(desc) {
        processTestCases(runner, testCases)
      }
    }
  }

  generateTestCases()
}
