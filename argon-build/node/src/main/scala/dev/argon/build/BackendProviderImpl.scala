package dev.argon.build

import cats.implicits._
import dev.argon.backend.js.JSBackend
import dev.argon.backend.module.ArModuleBackend
import dev.argon.backend.{Backend, ResourceReader, ResourceWriter}
import dev.argon.build.testrunner.js.JavaScriptNodeVMTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._

object BackendProviderImpl {

  def live: ULayer[BackendProvider] = ZLayer.succeed(
    new BackendProvider.Service {

      val allBackends = Vector(ArModuleBackend, JSBackend)

      override def findBackend(id: String): Option[Backend] =
        allBackends.find { _.id === id }

      override def testCaseRunners[I <: ResourceIndicator: Tagged, P: Path : Tagged](references: Vector[I], pathResolver: I => UIO[P]): Seq[TestCaseRunner[ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]]] =
        Seq(ParseTestCaseRunner) ++
          Seq(JSBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
          Seq(new JavaScriptNodeVMTestCaseRunner(references, pathResolver))
    }
  )

}
