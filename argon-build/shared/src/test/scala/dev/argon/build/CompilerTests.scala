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

object CompilerTests extends CompilerTestSuiteBase {
  override val suiteName: String = "Compiler Tests"
  override val testCases: TestCaseStructure = TestCaseLoader.findTestCases(TestCases.all)
  override val runners: Seq[TestCaseRunner] = PlatformHelpers.testCaseRunners(referencePaths)
}
