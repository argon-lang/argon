package dev.argon.build

import dev.argon.backend.{ResourceReader, ResourceWriter}
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

  type TestExecEnv = ResourceReader[TestResourceIndicator] with ResourceWriter[Nothing] with JSModuleLoad

  def fileIOLayer: ZLayer[Any, Nothing, FileIO] = ZLayer.succeed(FileIO.liveNode)

  def execEnvLayer: ZLayer[FileIO, Throwable, TestExecEnv] =
    TestResourceReader.layer.passthrough >>> (
        ZLayer.fromFunction[Has[TestResourceReader.Service], ResourceReader.Service[TestResourceIndicator]](_.get) ++
          ResourceWriter.forNothing ++
          TestJSModuleLoad.layer
      )

  def testCaseRunners(references: Vector[TestResourceIndicator]): Seq[TestCaseRunner[TestExecEnv]] =
    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
      Seq(new JavaScriptNodeVMTestCaseRunner(references))
}
