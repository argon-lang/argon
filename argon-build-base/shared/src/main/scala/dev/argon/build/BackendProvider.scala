package dev.argon.build

import dev.argon.backend.Backend
import dev.argon.build.testrunner.TestCaseRunner
import dev.argon.options.FileList
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.util.MaybeBlocking

object BackendProvider {

  trait Service {
    val allBackends: Seq[Backend]
    def findBackend(id: String): Option[Backend]
    def testCaseRunners(references: FileList): Seq[TestCaseRunner[FileIO with ZipRead with MaybeBlocking]]
  }

}
