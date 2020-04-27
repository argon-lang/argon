package dev.argon.build

import dev.argon.backend.ResourceAccess
import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.build.testrunner.js.{JSModuleLoad, JavaScriptNodeVMTestCaseRunner}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import zio._
import zio.console.Console
import zio.system.System

object PlatformHelpers {

  type TestExecEnv = ResourceAccess with JSModuleLoad

  def fileIOLayer: ZLayer[Any, Nothing, FileIO] = ZLayer.succeed(FileIO.liveNode)

  def execEnvLayer: ZLayer[FileIO, Throwable, TestExecEnv] =
    TestResourceAccess.layer.passthrough >>> (
        ZLayer.fromFunction[Has[TestResourceAccess.Service], ResourceAccess.Service](_.get) ++
          TestJSModuleLoad.layer
      )

  def testCaseRunners(references: Vector[ResourceIndicator]): Seq[TestCaseRunner[TestExecEnv]] =
    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
      Seq(new JavaScriptNodeVMTestCaseRunner(references))
}
