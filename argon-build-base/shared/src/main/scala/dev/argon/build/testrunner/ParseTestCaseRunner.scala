package dev.argon.build.testrunner

import zio.{Chunk, IO, Runtime, UIO, URIO, ZIO}
import dev.argon.build._
import cats.implicits._
import dev.argon.compiler.CompilationError
import dev.argon.compiler.loaders.SourceParser
import dev.argon.parser.impl.ArgonSourceParser
import dev.argon.util.{FileID, FileSpec}
import zio.stream.ZStream

object ParseTestCaseRunner extends TestCaseRunner[Any] {

  override val name: String = "Parsing"

  override def runTest(testCase: TestCase): IO[CompilationError, TestCaseCompletedResult] =
    ZStream.fromIterable(testCase.sourceCode)
        .zipWithIndex
        .flatMap { case (inputSource, index) =>
          val fileSpec = FileSpec(FileID(index.toInt), inputSource.name)

          val sourceCodeStream = ZStream.fromChunk(Chunk.fromArray(inputSource.data.toCharArray))

          ZStream.unwrap(ZIO.access[SourceParser](_.get.parse(fileSpec)(sourceCodeStream)))
        }
      .provideLayer(ArgonSourceParser.live)
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
