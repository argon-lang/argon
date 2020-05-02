package dev.argon.build

import dev.argon.backend.{Backend, ResourceReader, ResourceWriter}
import dev.argon.build.testrunner.TestCaseRunner
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path
import dev.argon.io.fileio.FileIO
import zio._

object BackendProvider {

  trait Service {
    def findBackend(id: String): Option[Backend]
    def testCaseRunners[I <: ResourceIndicator: Tagged, P: Path : Tagged](references: Vector[I], pathResolver: I => UIO[P]): Seq[TestCaseRunner[ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]]]
  }

}
