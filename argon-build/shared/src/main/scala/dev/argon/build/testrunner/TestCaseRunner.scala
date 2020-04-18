package dev.argon.build.testrunner

import dev.argon.compiler.CompilationError
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import zio.{IO, Runtime, ZIO}
import dev.argon.build._

trait TestCaseRunner {

  val name: String

  def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult]

  protected def isExpectedError(errors: NonEmptyList[CompilationError], errorName: String): Boolean =
    errors match {
      case NonEmptyList(error, Nil) =>
        val realName = error.getClass.getName
        val expectedName = CompilationError.getClass.getName + errorName

        realName.startsWith(expectedName) &&
          (expectedName.length === realName.length || realName.charAt(expectedName.length) === '$')

      case _ => false
    }

}
