package dev.argon.build

import dev.argon.backend.js.{JSBackend, JSModuleExtractorFactory}
import dev.argon.backend.generic.GenericBackend
import dev.argon.backend.Backend
import zio._
import cats.implicits._
import dev.argon.build.testrunner.js.GraalJSTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.options.FileList
import dev.argon.io.fileio.{FileIO, ZipRead}
import zio.blocking.Blocking

object BackendProviderImpl {

  def live: URLayer[ZipRead with Blocking with FileIO, BackendProvider] = ZLayer.fromEffect(
    for {
      blockingEnv <- ZIO.access[Blocking](_.get)
      jsBackend <- JSBackend.make
    } yield new BackendProvider.Service {
      override val allBackends = Vector(GenericBackend, jsBackend)

      override def findBackend(id: String): Option[Backend] =
        allBackends.find { _.id === id }

      override def testCaseRunners(references: FileList): Seq[TestCaseRunner[FileIO with ZipRead with Blocking]] =
        Seq(ParseTestCaseRunner) ++
          allBackends.map { backend => new BuildTestCaseRunner(backend, references) } ++
          Seq(new GraalJSTestCaseRunner(jsBackend, references, blockingEnv))
    }
  )

}
