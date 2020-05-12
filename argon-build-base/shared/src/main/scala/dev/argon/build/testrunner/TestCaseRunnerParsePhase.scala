package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.stream._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.{CompilationError, ErrorList}
import cats._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances._
import zio._
import dev.argon.build._
import dev.argon.stream.builder.ZStreamSource
import zio.stream.ZStream

private[testrunner] trait TestCaseRunnerParsePhase[-R] extends TestCaseRunner[R] {

  protected final def parseTestCaseSource(testCase: TestCase): IO[TestCaseError, Vector[SourceAST]] = {

    val inputFiles = ZStream.fromIterable[(InputSourceData, Int)](testCase.sourceCode.zipWithIndex)
      .map {
        case (InputSourceData(filename, data), i) =>
          InputFileInfo[Any, ErrorList](
            FileSpec(FileID(i), filename),
            new ZStreamSource(ZStream.fromChunk(Chunk.fromArray(data.toCharArray)))
          )
      }

    val parsedInputStream =
      BuildProcess.parseInput(new ZStreamSource(inputFiles))

    parsedInputStream.foldLeftM(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
      .map {
        case (parsedInput, _) => parsedInput
      }
      .mapError(compilationFailureResult)
  }

}
