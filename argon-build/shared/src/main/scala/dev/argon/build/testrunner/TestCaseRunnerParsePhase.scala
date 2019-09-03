package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.stream._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.{CompilationError, IOCompilation}
import cats._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances._
import dev.argon.io.FileIO
import zio._
import dev.argon.build._
import dev.argon.stream.builder.{Generator, Iter}
import zio.stream.ZStream
import dev.argon.stream.builder.ZStreamIter._

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  private type F[A] = ZIO[BuildEnvironment, NonEmptyList[CompilationError], A]
  private type IterStream[A, X] = (ZStream[BuildEnvironment, NonEmptyList[CompilationError], A], X)

  protected final def parseTestCaseSource(testCase: TestCase)(implicit ioComp: IOCompilation[BuildEnvironment]): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Vector[SourceAST]] = {

    val inputFiles = ZStream.fromIterable[(InputSourceData, Int)](testCase.sourceCode.zipWithIndex)
      .map {
        case (InputSourceData(filename, data), i) =>
          InputFileInfo[IterStream](
            FileSpec(FileID(i), filename),
            (ZStream.fromIterable(data.toVector), ())
          )
      }

    val parsedInputStream = {
      import zio.interop.catz._
      BuildProcess.parseInput[F, IterStream]((inputFiles, ()))
    }

    Iter[F, Generator[F, ?, ?], Unit].foldLeftM(parsedInputStream)(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
      .map {
        case (parsedInput, _) => parsedInput
      }

  }

}
