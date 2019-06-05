package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.stream._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.{CompilationError, IOCompilation}
import cats._
import cats.data.NonEmptyList
import cats.instances._
import scalaz.zio._
import scalaz.zio.blocking.Blocking

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  protected final def parseTestCaseSource(testCase: TestCase)(implicit ioComp: IOCompilation[Blocking]): ZIO[Blocking, NonEmptyList[CompilationError], Vector[SourceAST]] = {

    val inputFiles = ArStream.fromVector[ZIO, Blocking, NonEmptyList[CompilationError], (InputSourceData, Int)](testCase.sourceCode.zipWithIndex)
      .map {
        case (InputSourceData(filename, data), i) =>
          InputFileInfo(
            FileSpec(FileID(i), filename),
            ArStream.fromVector[ZIO, Blocking, NonEmptyList[CompilationError], Char](data.toVector)
          )
      }

    {
      import scalaz.zio.interop.catz._
      BuildProcess.parseInput[ZIO, Blocking](inputFiles)
    }.foldLeft(StreamTransformation.toVector[ZIO, Blocking, NonEmptyList[CompilationError], SourceAST])
  }

}
