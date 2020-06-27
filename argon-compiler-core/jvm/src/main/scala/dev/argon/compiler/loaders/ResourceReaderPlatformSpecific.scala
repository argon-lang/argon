package dev.argon.compiler.loaders

import java.lang.Runtime.{Version => JDKVersion}

import dev.argon.compiler.ErrorList
import dev.argon.io.JarFileReader
import zio.Managed

object ResourceReaderPlatformSpecific {
  trait Service[I <: ResourceIndicator] {
    def getJarReader(id: I, jdkVersion: JDKVersion): Managed[ErrorList, JarFileReader[Any, ErrorList]]
  }

  trait ForNothingService {
    def getJarReader(id: Nothing, jdkVersion: JDKVersion): Managed[ErrorList, JarFileReader[Any, ErrorList]] = id
  }
}
