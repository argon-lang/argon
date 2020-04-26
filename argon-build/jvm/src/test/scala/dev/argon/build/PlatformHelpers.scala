package dev.argon.build

import dev.argon.backend.Backend
import dev.argon.backend.js.JSBackend
import dev.argon.build.testrunner.js.GraalJSTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import zio.{IO, RIO, UIO, ZLayer, ZManaged}
import zio.blocking.Blocking
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def fileIOLayer: ZLayer[Blocking, Nothing, FileIO] = FileIO.live

  def testCaseRunners(references: RIO[FileIO, Vector[Path]]): Seq[TestCaseRunner] = {
    val resRefs = references.map(_.map(PathResourceIndicator.apply))

    Seq(ParseTestCaseRunner) ++
      Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, resRefs) } ++
      Seq(new GraalJSTestCaseRunner(resRefs))
  }

}
