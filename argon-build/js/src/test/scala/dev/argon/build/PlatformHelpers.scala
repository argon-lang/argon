package dev.argon.build

import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.build.testrunner.js.JavaScriptTestCaseRunner
import dev.argon.io.{NodeIOEnvironment, Path}
import zio.{UIO, ZManaged}
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def ioEnvironment: Console with System => NodeIOEnvironment = new NodeIOEnvironment(_)

  def testCaseRunners(referencePaths: UIO[Vector[Path]]): Seq[TestCaseRunner] =
    Seq(ParseTestCaseRunner)// ++
      //Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
      //Seq(JavaScriptTestCaseRunner(references))
}
