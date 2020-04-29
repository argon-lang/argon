package dev.argon.build

import java.io.{FileNotFoundException, IOException}

import cats.data.NonEmptyList
import dev.argon.backend.{ResourceAccess, ResourceReader}
import dev.argon.build.testrunner.js.JSModuleLoad
import dev.argon.compiler.{Comp, CompilationError, CompilationMessageSource, ErrorList}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object TestResourceReader {

  def layer: ZLayer[FileIO, Throwable, Has[Service]] =
    ZLayer.fromManaged(ZManaged.accessManaged[FileIO] { env =>
      val fileIO = env.get

      for {
        libDir <- ZManaged.fromEffect(
          fileIO.getEnv("ARGON_LIB_DIR")
            .get
            .mapError(_ => new RuntimeException("ARGON_LIB_DIR was not set"))
        )

        liveResReaderEnv <- ResourceReader.forFileIO.build
        liveResReader = liveResReaderEnv.get
      } yield (new Service {

        private def ioFail(ex: IOException): IO[ErrorList, Nothing] =
          IO.fail(NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex))))

        private def managedFail(ex: IOException): Managed[ErrorList, Nothing] =
          Managed.fromEffect(ioFail(ex))

        override def getLibPath(name: String): UIO[Path] =
          Path.of(libDir, name, name + ".armodule")

        override type ZipReader = liveResReader.ZipReader

        override def getZipReader(id: TestResourceIndicator): Managed[ErrorList, ZipReader] =
          id match {
            case InputFileResourceIndicator(_) => managedFail(new IOException("Input file cannot be used as zip file."))
            case LibraryResourceIndicator(name) =>
              ZManaged.fromEffect(getLibPath(name))
                .flatMap { path =>
                  liveResReader.getZipReader(PathResourceIndicator(path))
                }
          }

        override def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit] =
          liveResReader.zipEntryStream(zip, name)

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A] =
          liveResReader.deserializeProtocolBuffer(companion)(data)
      } : Service)
    })

  trait Service extends ResourceReader.Service[TestResourceIndicator] {
    def getLibPath(name: String): UIO[Path]
  }

}
