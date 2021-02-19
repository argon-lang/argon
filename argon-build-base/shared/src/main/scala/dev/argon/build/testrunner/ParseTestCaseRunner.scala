package dev.argon.build.testrunner

import zio.{Chunk, IO, Runtime, UIO, URIO, ZIO}
import dev.argon.build._
import cats.implicits._
import dev.argon.compiler.CompilationError
import dev.argon.parser.impl.ArgonSourceParser
import dev.argon.util.{FileID, FileSpec}
import zio.stream.ZStream

object ParseTestCaseRunner extends TestCaseRunner[Any] {

  override val name: String = "Parsing"



  override def runTest(testCase: TestCase): IO[CompilationError, TestCaseCompletedResult] =
    ZStream.fromIterable(testCase.sourceCode.zipWithIndex)
      .flatMap { case (inputSourceData, index) =>
        val inputFile = FileSpec(FileID(index), inputSourceData.name)

        val fileStream = ZStream.fromChunk(Chunk.fromArray(inputSourceData.data.toCharArray))

        ArgonSourceParser.parse(inputFile)(fileStream)
      }
      .runDrain
      .as(TestCaseActualResult.NotExecuted)

  override def isResultExpected(actual: TestCaseActualResult, expected: TestCaseExpectedResult): Boolean =
    expected match {
      case TestCaseExpectedError("SyntaxCompilerError") =>
        super.isResultExpected(actual, expected)

      case _ =>
        super.isResultExpected(actual, TestCaseExpectedAnyOutput)
    }
}
