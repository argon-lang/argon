package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.build.testrunner._
import com.mi3software.argon.build.testrunner.node.{NodeLauncher, NodeTestCaseRunner}
import org.scalatest.{BeforeAndAfterAll, FunSpec, Matchers}
import scalaz.zio.DefaultRuntime

class CompilerTests extends FunSpec with DefaultRuntime with Matchers with BeforeAndAfterAll {

  private def processTestCases(runner: TestCaseRunner, structure: TestCaseStructure): Unit = {

    for((name, nestedStructures) <- structure.nestedStructures) {
      describe(name) {
        processTestCases(runner, nestedStructures)
      }
    }

    for(testCase <- structure.tests) {
      it(testCase.name) {
        unsafeRun(runner.runTest(testCase)) shouldBe TestCaseResult.Success
      }
    }

  }

  private val libraries = Vector(
    "Argon.Core",
  )

  private val nodeLauncher = new NodeLauncher("external-api/node-api/bin/index.js")

  private val references = libraries.map { name => new File(s"libraries/$name/$name.armodule") }

  private def generateTestCases(): Unit = {
    val testCases = unsafeRun(TestCaseLoader.findTestCases(new File(getClass.getResource("/com/mi3software/argon/compiler/testcases/").toURI)))

    val runners = Vector(
      "Parsing" -> ParseTestCaseRunner,
    ) ++ Backend.allBackends.map { backend =>
      s"Compilation (${backend.name})" -> new BuildTestCaseRunner(backend, references)
    } ++ Vector(
      "Node Execution" -> new NodeTestCaseRunner(references, nodeLauncher)
    )

    runners foreach { case (desc, runner) =>
      describe(desc) {
        processTestCases(runner, testCases)
      }
    }
  }

  generateTestCases()

  override protected def afterAll(): Unit = {
    nodeLauncher.close()
  }
}
