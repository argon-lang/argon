package dev.argon.build

import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.js.GraalJSTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.compiler.backend.Backend
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio.{IO, RIO, UIO, ZLayer, ZManaged}
import zio.blocking.Blocking
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def fileIOLayer: ZLayer[Blocking, Nothing, FileIO] = FileIO.live

  def testCaseRunners(references: RIO[FileIO, Vector[Path]]): Seq[TestCaseRunner] =
    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
      Seq(new GraalJSTestCaseRunner(references))

}
