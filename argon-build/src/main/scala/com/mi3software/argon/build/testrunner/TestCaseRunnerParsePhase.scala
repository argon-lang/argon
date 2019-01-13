package com.mi3software.argon.build.testrunner

import com.mi3software.argon.build._
import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.parser.SourceAST
import com.mi3software.argon.util.{FileID, FileSpec}
import scalaz.{EitherT, NonEmptyList}
import scalaz.effect.IO
import fs2.Stream
import shims.effect._

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  protected final def parseTestCaseSource(testCase: TestCase): EitherT[IO, NonEmptyList[CompilationError], Vector[SourceAST]] =
    new BuildProcess.ParsePhase[IO] {
      override protected def findInputFiles: fs2.Stream[IO, InputFileInfo[IO]] =
        Stream(testCase.sourceCode.zipWithIndex: _*)
          .covary[IO]
          .map {
            case (InputSourceData(filename, data), i) =>
              InputFileInfo(
                FileSpec(FileID(i), filename),
                Stream(data: _*).covary[IO]
              )
          }

    }.parseInput

}
