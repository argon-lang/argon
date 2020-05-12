package dev.argon.build

import java.io.{FileNotFoundException, IOException}

import cats.data.NonEmptyList
import dev.argon.backend.{ResourceAccess, ResourceReader}
import dev.argon.compiler.{Comp, CompilationError, CompilationMessageSource, ErrorList}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.system.System
import zio.test.environment._

object TestResourceReader {

  def layer[P : Path : Tagged]: ZLayer[FileIO[P] with FileIOLite with Live, Throwable, Has[Service[P]]] =
    ZLayer.fromManaged(ZManaged.accessManaged[FileIO[P] with FileIOLite with Live] { env =>
      for {
        libDir <- ZManaged.fromEffect(
          live(system.env("ARGON_LIB_DIR"))
            .orDie
            .get
            .mapError { _ => new RuntimeException("ARGON_LIB_DIR was not set") }
        )

        liveResReaderEnv <- ResourceReader.forFileIO.build
        liveResReader = liveResReaderEnv.get
      } yield (new Service[P] {

        override def getLibPath(name: String): UIO[P] =
          Path.of(libDir, name, name + ".armodule")

        override type ZipReader = liveResReader.ZipReader

        override def getZipReader(id: TestResourceIndicator): Managed[ErrorList, ZipReader] =
          id match {
            case LibraryResourceIndicator(name) =>
              ZManaged.fromEffect(getLibPath(name))
                .flatMap { path =>
                  liveResReader.getZipReader(PathResourceIndicator(path))
                }
          }

        override def zipEntryStream(zip: ZipReader, name: String): Source[Any, ErrorList, Chunk[Byte], Unit] =
          liveResReader.zipEntryStream(zip, name)

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Any, ErrorList, Chunk[Byte], Unit]): Comp[A] =
          liveResReader.deserializeProtocolBuffer(companion)(data)
      } : Service[P])
    })

  trait Service[P] extends ResourceReader.Service[TestResourceIndicator] {
    def getLibPath(name: String): UIO[P]
  }

}
