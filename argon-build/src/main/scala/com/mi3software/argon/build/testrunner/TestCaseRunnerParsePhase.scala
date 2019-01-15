package com.mi3software.argon.build.testrunner

import com.mi3software.argon.build._
import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.parser.SourceAST
import com.mi3software.argon.util.stream.ArStream
import com.mi3software.argon.util.{FileID, FileSpec}
import scalaz.{EitherT, NonEmptyList}
import scalaz.effect.IO
import com.mi3software.argon.compiler.IOCompilation

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  protected final def parseTestCaseSource(testCase: TestCase)(implicit ioComp: IOCompilation): IO[Vector[SourceAST]] =
    BuildProcess.parseInput[IO](
      ArStream.fromVector[IO, (InputSourceData, Int), Unit](testCase.sourceCode.zipWithIndex, ())
        .mapItems {
          case (InputSourceData(filename, data), i) =>
            InputFileInfo(
              FileSpec(FileID(i), filename),
              ArStream.fromVector[IO, Char, Unit](data.toVector, ())
            )
        }
    ).toVector

}
