package com.mi3software.argon.build.testrunner


import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.util.{CatsInstances, FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.SyntaxError
import fs2._
import com.mi3software.argon.parser.impl.ParseHandler
import shims._

object BuildTestCaseRunner extends TestCaseRunner {
  override def runTest[F[_] : cats.effect.Sync](testCase: TestCase): F[TestCaseResult] =
    testCase
      .sourceCode
      .zipWithIndex
      .traverse {
        case (InputSourceData(filename, data), i) =>
          val fileSpec = FileSpec(FileID(i), filename)

          Stream(data: _*)
            .covary[EitherT[F, NonEmptyList[SyntaxError], ?]]
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
