package dev.argon.build

import dev.argon.backend.js.{JSBackend, JSModuleExtractorFactory}
import dev.argon.backend.module.ArModuleBackend
import dev.argon.backend.{Backend, ResourceWriter}
import zio._
import cats.implicits._
import dev.argon.build.testrunner.js.GraalJSTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio.blocking.Blocking

object BackendProviderImpl {

  def live: URLayer[Blocking, BackendProvider] = ZLayer.fromEffect(
    JSModuleExtractorFactory.make.map { jsModuleExtractor =>
      new BackendProvider.Service {

        val jsBackend = JSBackend(jsModuleExtractor)
        override val allBackends = Vector(ArModuleBackend, jsBackend)

        override def findBackend(id: String): Option[Backend] =
          allBackends.find { _.id === id }

        override def testCaseRunners[I <: ResourceIndicator: Tag, P: Path : Tag](references: Vector[I], pathResolver: I => UIO[P]): Seq[TestCaseRunner[ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]]] =
          Seq(ParseTestCaseRunner) ++
            allBackends.map { backend => new BuildTestCaseRunner(backend, references) } ++
            Seq(new GraalJSTestCaseRunner(jsBackend, references, pathResolver))
    }
  )

}
