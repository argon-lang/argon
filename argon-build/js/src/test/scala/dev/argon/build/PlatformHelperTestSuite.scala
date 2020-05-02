package dev.argon.build

import dev.argon.backend.{ResourceReader, ResourceWriter}
import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.js.{JSModuleLoad, JavaScriptNodeVMTestCaseRunner}
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.platform._
import zio.{Has, IO, RIO, UIO, ZLayer, ZManaged}
import zio.console.Console
import zio.system.System

trait PlatformHelperTestSuite extends PlatformRunnableSpec {

  protected type TestExecEnv = ResourceReader[TestResourceIndicator] with ResourceWriter[Nothing] with JSModuleLoad

  protected def execEnvLayer: ZLayer[Environment, Throwable, TestExecEnv] =
    TestResourceReader.layer[FilePath].passthrough >>> (
      ZLayer.fromFunction[Has[TestResourceReader.Service[FilePath]], ResourceReader.Service[TestResourceIndicator]](_.get) ++
        ResourceWriter.forNothing ++
        TestJSModuleLoad.layer
      )

  protected def runners(references: Vector[TestResourceIndicator]): Seq[TestCaseRunner[TestExecEnv]] =
    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
      Seq(new JavaScriptNodeVMTestCaseRunner(references))

}
