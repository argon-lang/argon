package dev.argon.build

import cats.implicits._
import dev.argon.backend.js.{JSBackend, JSModuleExtractorImpl}
import dev.argon.backend.module.ArModuleBackend
import dev.argon.backend.{Backend, ResourceWriter}
import dev.argon.build.testrunner.js.JavaScriptNodeVMTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._

object BackendProviderImpl {

  def live: ULayer[BackendProvider] = ZLayer.succeed(
    new BackendProvider.Service {

      val jsBackend = JSBackend(JSModuleExtractorImpl)
      override val allBackends = Vector(ArModuleBackend, jsBackend)

      override def findBackend(id: String): Option[Backend] =
        allBackends.find { _.id === id }

      override def testCaseRunners[I <: ResourceIndicator: Tag, P: Path : Tag](references: Vector[I], pathResolver: I => UIO[P]): Seq[TestCaseRunner[ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]]] =
        Seq(ParseTestCaseRunner) ++
          Seq(jsBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
          Seq(new JavaScriptNodeVMTestCaseRunner(jsBackend, references, pathResolver))
    }
  )

}
