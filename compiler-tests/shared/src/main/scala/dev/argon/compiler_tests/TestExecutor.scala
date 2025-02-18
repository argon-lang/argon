package dev.argon.compiler_tests

import dev.argon.compiler.*
import zio.*
import dev.argon.backend.Backend

trait TestExecutor {
  val backend: Backend[TestError]
  type TestProgram

  def toTestProgram(program: backend.Output): IO[TestError, TestProgram]
  def run(program: TestProgram, libraries: Map[TubeName, TestProgram]): Task[String]
}

object TestExecutor {
  type Aux[B <: Backend[TestError]] = TestExecutor { val backend: B }
}
