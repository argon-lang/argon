package dev.argon.build

import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.build.testrunner.js.JavaScriptNodeVMTestCaseRunner
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def fileIOLayer: ZLayer[Any, Nothing, FileIO] = ZLayer.succeed(FileIO.liveNode)

  def testCaseRunners(referencePaths: RIO[FileIO, Vector[Path]]): Seq[TestCaseRunner] =
    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, referencePaths) } ++
      Seq(JavaScriptNodeVMTestCaseRunner(referencePaths))
}
