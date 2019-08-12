package dev.argon.build

import java.io.File

import dev.argon.build.testrunner._
import dev.argon.build.testrunner.node.{NodeLauncher, NodeTestCaseRunner}
import dev.argon.io.IOEnvironment
import org.scalatest.{BeforeAndAfterAll, FunSpec, Matchers}
import zio._

class CompilerTests extends FunSpec with DefaultRuntime with Matchers with BeforeAndAfterAll {

  private def unsafeRunBuild[E, A](action: ZIO[BuildEnvironment, E, A]): A =
    unsafeRun(
      action.provideSome(new IOEnvironment(_))
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

  private val nodeLauncher = unsafeRunBuild(NodeLauncher(this, "external-api/node-api/bin/index.js"))

  private val references = libraries.map { name => new File(s"libraries/$name/$name.armodule") }

  private def generateTestCases(): Unit = {
    val testCases = unsafeRunBuild(TestCaseLoader.findTestCases(new File(getClass.getResource("/dev/argon/compiler/testcases").toURI)))

    val runners = Vector(
      "Parsing" -> ParseTestCaseRunner,
    ) ++ Backends.allBackends.filterNot(_.id === "argon-module").map { backend =>
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
    unsafeRunBuild(nodeLauncher.close)
  }
}
