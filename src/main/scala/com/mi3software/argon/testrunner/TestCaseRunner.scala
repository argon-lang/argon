package com.mi3software.argon.testrunner

import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.parser.{ParseHandler, SourceAST, SyntaxErrorData}
import com.mi3software.argon.util.{CharSequenceHandlerProvider, FileID, FileSpec, WithSource}

import scalaz._
import Scalaz._

final class TestCaseRunner {

  private val parseHandler = new ParseHandler

  def parse(testCase: TestCase): TestCaseResult =
    testCase.sourceCode.zipWithIndex.traverse[NonEmptyList[SyntaxErrorData] \/ ?, Vector[WithSource[SourceAST]]] {
      case (InputSourceData(filename, data), i) =>
        parseHandler.parse(FileSpec(FileID(i), filename))(data)
    } match {
      case \/-(_) => TestCaseResult.Success
      case -\/(errors) =>
        TestCaseResult.Failure(
          TestCaseActualResult.Errors(errors.toVector.map(CompilationError.SyntaxCompilerError.apply)),
          TestCaseExpectedOutput("")
        )
    }

}
