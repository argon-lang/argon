package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.stream._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.{CompilationError, IOCompilation}
import cats._
import cats.data.NonEmptyList
import cats.instances._
import dev.argon.io.FileIO
import zio._
import zio.blocking.Blocking
import dev.argon.build._

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  protected final def parseTestCaseSource(testCase: TestCase)(implicit ioComp: IOCompilation[BuildEnvironment]): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Vector[SourceAST]] = {

    val inputFiles = ArStream.fromVector[ZIO, BuildEnvironment, NonEmptyList[CompilationError], (InputSourceData, Int)](testCase.sourceCode.zipWithIndex)
      .map {
        case (InputSourceData(filename, data), i) =>
          InputFileInfo(
            FileSpec(FileID(i), filename),
            ArStream.fromVector[ZIO, Blocking, NonEmptyList[CompilationError], Char](data.toVector)
          )
      }

    {
      import zio.interop.catz._
      BuildProcess.parseInput[ZIO, BuildEnvironment](inputFiles)
    }.foldLeft(StreamTransformation.toVector[ZIO, BuildEnvironment, NonEmptyList[CompilationError], SourceAST])
  }

}
