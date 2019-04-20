package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import scalaz._
import Scalaz._
import scalaz.zio.IO

trait TestCaseRunner {
  def runTest(testCase: TestCase): IO[Throwable, TestCaseResult]

  protected def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, INil()) =>
        error.getClass.getName === CompilationError.getClass.getName + errorName

      case _ => false
    }
}
