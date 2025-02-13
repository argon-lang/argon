package dev.argon.compiler_tests

import dev.argon.compiler.*
import zio.*
import dev.argon.backend.Backend

trait TestExecutor {
  val backend: Backend
  type TestProgram

  def toTestProgram[E >: TestException](program: backend.Output[E]): IO[E, TestProgram]
  def run(program: TestProgram, libraries: Map[TubeName, TestProgram]): Task[String]
}

object TestExecutor {
  type Aux[B <: Backend] = TestExecutor { val backend: B }
}
