package dev.argon.compiler.loaders

import java.lang.Runtime.{Version => JDKVersion}

import dev.argon.compiler.CompError
import dev.argon.io.JarFileReader
import zio.Managed

object ResourceReaderPlatformSpecific {
  trait Service[I <: ResourceIndicator] {
    def getJarReader(id: I, jdkVersion: JDKVersion): Managed[CompError, JarFileReader[Any, CompError]]
  }

  trait ForNothingService {
    def getJarReader(id: Nothing, jdkVersion: JDKVersion): Managed[CompError, JarFileReader[Any, CompError]] = id
  }
}
