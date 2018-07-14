package com.mi3software.argon.testrunner

import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.util.{FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.impl.ParseHandler

final class TestCaseRunner {

  def parse(testCase: TestCase): TestCaseResult =
    testCase.sourceCode.zipWithIndex.traverseU {
      case (InputSourceData(filename, data), i) =>
        ParseHandler.parse(FileSpec(FileID(i), filename))(data)
    } match {
      case \/-(_) => TestCaseResult.Success
      case -\/(errors) =>
        TestCaseResult.Failure(
          TestCaseActualResult.Errors(errors.toVector.map(CompilationError.SyntaxCompilerError.apply)),
          TestCaseExpectedOutput("")
        )
    }

}
