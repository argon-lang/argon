package dev.argon.plugin.executor

import dev.argon.compiler.tube.TubeName

trait ExecutionTestCase[Options, CompileOutput] {
  val testTube: TestTubeInfo[Options, CompileOutput]
  val libraries: Map[TubeName, TestTubeInfo[Options, CompileOutput]]
}
