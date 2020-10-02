package dev.argon.backend

import java.io.IOException

import dev.argon.compiler._
import dev.argon.io.JarFileReader
import zio._
import java.lang.Runtime.{Version => JDKVersion}

import dev.argon.io.fileio.FileIO

object PathResourceIndicatorPlatformSpecific {
  trait ReaderService[P] {
    protected val fileIO: FileIO.Service[P]

    def getJarReader(id: PathResourceIndicator[P], jdkVersion: JDKVersion): Managed[CompilationError, JarFileReader[Any, CompilationError]] =
      fileIO.openJarFile(Compilation.unwrapThrowableCause)(id.path, jdkVersion)
  }
}
