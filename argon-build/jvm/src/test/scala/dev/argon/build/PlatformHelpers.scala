package dev.argon.build

import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.js.JavaScriptRPCTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.compiler.backend.Backend
import dev.argon.io.{IOEnvironment, Path}
import zio.{UIO, ZManaged}
import zio.blocking.Blocking
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def ioEnvironment: Blocking with Console with System => IOEnvironment = new IOEnvironment(_)

  def testCaseRunners(references: UIO[Vector[Path]]): Seq[TestCaseRunner] =
    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
      Seq(JavaScriptRPCTestCaseRunner("external-api/node-api/bin/index.js")(references))

}
