package dev.argon.build

import cats.implicits._
import dev.argon.backend.js.JSBackend
import dev.argon.backend.generic.GenericBackend
import dev.argon.backend.Backend
import dev.argon.build.testrunner.js.JavaScriptNodeVMTestCaseRunner
import dev.argon.build.testrunner.{BuildTestCaseRunner, ParseTestCaseRunner, TestCaseRunner}
import dev.argon.options.FileList
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.util.MaybeBlocking
import zio._

object BackendProviderImpl {

  def live: URLayer[ZipRead with FileIO, BackendProvider] = ZLayer.fromEffect(
    for {
      jsBackend <- JSBackend.make
    } yield new BackendProvider.Service {
      override val allBackends = Vector(GenericBackend, jsBackend)

      override def findBackend(id: String): Option[Backend] =
        allBackends.find { _.id === id }

      override def testCaseRunners(references: FileList): Seq[TestCaseRunner[FileIO with ZipRead with MaybeBlocking]] =
        Seq(ParseTestCaseRunner) ++
          Seq(jsBackend).map { backend => new BuildTestCaseRunner(backend, references) } ++
          Seq(new JavaScriptNodeVMTestCaseRunner(jsBackend, references))

    }
  )

}
