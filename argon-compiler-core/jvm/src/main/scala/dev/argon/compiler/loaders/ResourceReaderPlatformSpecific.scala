package dev.argon.compiler.loaders

import java.lang.Runtime.{Version => JDKVersion}

import dev.argon.compiler.CompilationError
import dev.argon.io.JarFileReader
import zio.Managed

object ResourceReaderPlatformSpecific {
  trait Service[I <: ResourceIndicator] {
    def getJarReader(id: I, jdkVersion: JDKVersion): Managed[CompilationError, JarFileReader[Any, CompilationError]]
  }

  trait ForNothingService {
    def getJarReader(id: Nothing, jdkVersion: JDKVersion): Managed[CompilationError, JarFileReader[Any, CompilationError]] = id
  }
}
