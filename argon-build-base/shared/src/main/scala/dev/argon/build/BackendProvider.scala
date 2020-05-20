package dev.argon.build

import dev.argon.backend.{Backend, ResourceWriter}
import dev.argon.build.testrunner.TestCaseRunner
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._

object BackendProvider {

  trait Service {
    val allBackends: Seq[Backend]
    def findBackend(id: String): Option[Backend]
    def testCaseRunners[I <: ResourceIndicator: Tag, P: Path : Tag](references: Vector[I], pathResolver: I => UIO[P]): Seq[TestCaseRunner[ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]]]
  }

}
