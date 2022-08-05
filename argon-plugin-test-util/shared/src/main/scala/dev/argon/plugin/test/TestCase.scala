package dev.argon.plugin.test

case class TestCase
(
  name: String,
  inputSources: Map[String, String],
  expectedResult: TestCase.ExpectedResult
)

object TestCase:
  enum ExpectedResult {
    case Output(text: String)
  }
end TestCase

