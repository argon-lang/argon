package dev.argon.build.testrunner

import scala.collection.immutable._

final case class TestCase(name: String, enabled: Boolean, sourceCode: Vector[InputSourceData], expectedResult: TestCaseExpectedResult)

sealed trait TestCaseExpectedResult
case object TestCaseExpectedAnyOutput extends TestCaseExpectedResult
final case class TestCaseExpectedOutput(text: String) extends TestCaseExpectedResult
final case class TestCaseExpectedError(errorName: String) extends TestCaseExpectedResult

