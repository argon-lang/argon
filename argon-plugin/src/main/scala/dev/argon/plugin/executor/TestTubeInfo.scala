package dev.argon.plugin.executor

trait TestTubeInfo[Options, CompileOutput] {
  def options: Options
  def compileOutput: CompileOutput
}
