package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import scalaz.zio.{IO, Runtime}

trait TestCaseRunner {
  def runTest(testCase: TestCase): IO[Throwable, TestCaseResult]

  protected def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, Nil) =>
        error.getClass.getName === CompilationError.getClass.getName + errorName

      case _ => false
    }
}
