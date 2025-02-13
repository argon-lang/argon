package dev.argon.compiler_tests.executors

import dev.argon.compiler.TubeName
import zio.*

trait JSExecutorPlatformSpecific {
  self: JSExecutor =>

  override def run(program: TestProgram, libraries: Map[TubeName, TestProgram]): Task[String] =
    ???
}
