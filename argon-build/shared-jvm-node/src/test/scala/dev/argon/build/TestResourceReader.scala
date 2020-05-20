package dev.argon.build

import java.io.{FileNotFoundException, IOException}

import cats.data.NonEmptyList
import dev.argon.backend.ResourceAccess
import dev.argon.compiler.{Comp, Compilation, CompilationError, CompilationMessageSource, ErrorList}
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.{Path, ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream._
import zio.system.System
import zio.test.environment._

object TestResourceReader {

  def layer[P : Path : Tag]: ZLayer[FileIO[P] with FileIOLite with Live, Throwable, Has[Service[P]]] =
    ZLayer.fromManaged(ZManaged.accessManaged[FileIO[P] with FileIOLite with Live] { env =>
      for {
        libDir <- ZManaged.fromEffect(
          live(system.env("ARGON_LIB_DIR"))
            .orDie
            .get
            .mapError { _ => new RuntimeException("ARGON_LIB_DIR was not set") }
        )

        liveResReaderEnv <- PathResourceIndicator.pathResourceReader.build
        liveResReader = liveResReaderEnv.get
      } yield (new Service[P] {

        override def getLibPath(name: String): UIO[P] =
          Path.of(libDir, name, "bin", name + ".armodule")


        override def readTextFile(id: TestResourceIndicator): Stream[ErrorList, Char] =
          ZStream.unwrap(readTextFileAsString(id).map { ZStream.fromIterable(_) })

        override def readTextFileAsString(id: TestResourceIndicator): Comp[String] =
          id match {
            case LibraryResourceIndicator(_) =>
              Compilation.forErrors(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(new FileNotFoundException())))
          }

        override def getZipReader(id: TestResourceIndicator): Managed[ErrorList, ZipFileReader[Any, ErrorList]] =
          id match {
            case LibraryResourceIndicator(name) =>
              ZManaged.fromEffect(getLibPath(name))
                .flatMap { path =>
                  liveResReader.getZipReader(PathResourceIndicator(path))
                }
          }

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Stream[ErrorList, Byte]): Comp[A] =
          liveResReader.deserializeProtocolBuffer(companion)(data)
      } : Service[P])
    })

  trait Service[P] extends ResourceReader.Service[TestResourceIndicator] {
    def getLibPath(name: String): UIO[P]
  }

}
