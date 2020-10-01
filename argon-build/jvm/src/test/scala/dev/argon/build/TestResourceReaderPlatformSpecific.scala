package dev.argon.build

import dev.argon.backend.PathResourceIndicator
import dev.argon.compiler.CompilationError
import dev.argon.io.{JarFileReader, Path}
import zio.{Managed, UIO, ZManaged}
import java.lang.Runtime.{Version => JDKVersion}

import dev.argon.compiler.loaders.ResourceReader

object TestResourceReaderPlatformSpecific {

  trait Service[P] {
    protected val liveResReader: ResourceReader.Service[PathResourceIndicator[P]]
    def getLibPath(name: String): UIO[P]
    protected implicit val pathInstance: Path[P]


    def getJarReader(id: TestResourceIndicator, jdkVersion: JDKVersion): Managed[CompilationError, JarFileReader[Any, CompilationError]] =
      id match {
        case LibraryResourceIndicator(name) =>
          ZManaged.fromEffect(getLibPath(name))
            .flatMap { path =>
              liveResReader.getJarReader(PathResourceIndicator(path), jdkVersion)
            }
      }
  }

}
