package dev.argon.build.testrunner

import dev.argon.build._
import dev.argon.parser.SourceAST
import dev.argon.util.stream.ArStream
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.IOCompilation
import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

private[testrunner] trait TestCaseRunnerParsePhase extends TestCaseRunner {

  protected final def parseTestCaseSource(testCase: TestCase)(implicit ioComp: IOCompilation): IO[Throwable, Vector[SourceAST]] =
    BuildProcess.parseInput[IO[Throwable, ?]](
      ArStream.fromVector[IO[Throwable, ?], (InputSourceData, Int), Unit](testCase.sourceCode.zipWithIndex, ())
        .mapItems {
          case (InputSourceData(filename, data), i) =>
            InputFileInfo(
              FileSpec(FileID(i), filename),
              ArStream.fromVector[IO[Throwable, ?], Char, Unit](data.toVector, ())
            )
        }(ioComp)
    ).toVector(ioComp)

}
