package com.mi3software.argon.build.testrunner

import com.mi3software.argon.compiler.CompilationError
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait TestCaseRunner {
  def runTest(testCase: TestCase): IO[TestCaseResult]

  protected def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, INil()) =>
        error.getClass.getName === CompilationError.getClass.getName + errorName

      case _ => false
    }
}
