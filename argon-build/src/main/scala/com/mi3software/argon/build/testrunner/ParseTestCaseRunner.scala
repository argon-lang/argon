package com.mi3software.argon.build.testrunner

import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.util.{CatsInstances, FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.SyntaxError
import fs2._
import com.mi3software.argon.parser.impl.ParseHandler
import scalaz.effect.IO
import shims._
import shims.effect._

object ParseTestCaseRunner extends TestCaseRunner {

  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    testCase
      .sourceCode
      .zipWithIndex
      .traverse {
        case (InputSourceData(filename, data), i) =>
          val fileSpec = FileSpec(FileID(i), filename)

          Stream(data: _*)
            .covary[EitherT[IO, NonEmptyList[SyntaxError], ?]]
            .through(ParseHandler.parse(fileSpec))
            .translate(ParseHandler.convertSyntaxErrorToCompilationError(fileSpec))
            .compile
            .toVector(CatsInstances.scalazEitherTSync)
      }
      .run
      .map {
        case \/-(_) => TestCaseResult.Success
        case -\/(errors) =>
          TestCaseResult.Failure(
            TestCaseActualResult.Errors(errors.map(CompilationError.SyntaxCompilerError.apply)),
            TestCaseExpectedOutput("")
          )
      }
}
