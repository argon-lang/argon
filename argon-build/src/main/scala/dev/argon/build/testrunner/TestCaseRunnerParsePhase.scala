package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.util.stream._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.{CompilationError, IOCompilation}
import cats._
import cats.data.NonEmptyList
import cats.instances._
import scalaz.zio._

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  protected final def parseTestCaseSource(testCase: TestCase)(implicit ioComp: IOCompilation): IO[NonEmptyList[CompilationError], Vector[SourceAST]] =
    BuildProcess.parseInput[IO](
      ArStream.fromVector[IO, NonEmptyList[CompilationError], (InputSourceData, Int)](testCase.sourceCode.zipWithIndex)
        .map {
          case (InputSourceData(filename, data), i) =>
            InputFileInfo(
              FileSpec(FileID(i), filename),
              ArStream.fromVector[IO, NonEmptyList[CompilationError], Char](data.toVector)
            )
        }
    ).foldLeft(StreamTransformation.toVector[IO, NonEmptyList[CompilationError], SourceAST])

}
