package com.mi3software.argon.build.testrunner

import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.util.{CatsInstances, FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.{SourceAST, SyntaxError}
import fs2._
import com.mi3software.argon.parser.impl.ParseHandler
import scalaz.effect.IO
import shims._
import shims.effect._


trait TestCaseRunner {
  def runTest(testCase: TestCase): IO[TestCaseResult]
}

object TestCaseRunner {

  def parseInput(testCase: TestCase): EitherT[IO, NonEmptyList[CompilationError], Vector[SourceAST]] =
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
      .leftMap { _.map[CompilationError](CompilationError.SyntaxCompilerError) }
      .map { _.flatten }

  def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, INil()) =>
        error.getClass.getName === CompilationError.getClass.getName + errorName

      case _ => false
    }


}
