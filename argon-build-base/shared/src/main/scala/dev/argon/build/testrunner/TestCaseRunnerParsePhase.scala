package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.stream._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.CompilationError
import cats._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances._
import zio._
import dev.argon.build._
import dev.argon.stream.builder.ZStreamSource
import zio.stream.ZStream

private[testrunner] trait TestCaseRunnerParsePhase[-R] extends TestCaseRunner[R] {

  private type F[A] = IO[NonEmptyList[CompilationError], A]

  protected final def parseTestCaseSource(testCase: TestCase): IO[TestCaseError, Vector[SourceAST]] = {

    val inputFiles = ZStream.fromIterable[(InputSourceData, Int)](testCase.sourceCode.zipWithIndex)
      .map {
        case (InputSourceData(filename, data), i) =>
          InputFileInfo[F](
            FileSpec(FileID(i), filename),
            ZStreamSource(ZStream.fromChunk(Chunk.fromArray(data.toCharArray)))
          )
      }

    val parsedInputStream =
      BuildProcess.parseInput(ZStreamSource(inputFiles))

    parsedInputStream.foldLeftM(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
      .map {
        case (parsedInput, _) => parsedInput
      }
      .mapError(compilationFailureResult)
  }

}
