package dev.argon.backend

import dev.argon.backend.Backend
import dev.argon.compiler.*
import zio.*

trait TestExecutor[E, Output] {
  type TestProgram

  def toTestProgram(program: Output): IO[E, TestProgram]
  def run(program: TestProgram, libraries: Map[TubeName, TestProgram]): Task[String]
}

