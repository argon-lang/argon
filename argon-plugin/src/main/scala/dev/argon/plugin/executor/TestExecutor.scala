package dev.argon.plugin.executor

import zio.*

trait TestExecutor[-R, +E, Options, CompileOutput] {
  def executeTest(testCase: ExecutionTestCase[Options, CompileOutput]): ZIO[R, E, ExecutionResult]
}
