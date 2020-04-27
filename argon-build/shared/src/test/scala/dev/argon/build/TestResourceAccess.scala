package dev.argon.build

import java.io.{FileNotFoundException, IOException}

import cats.data.NonEmptyList
import dev.argon.backend.ResourceAccess
import dev.argon.build.testrunner.js.JSModuleLoad
import dev.argon.compiler.{Comp, CompilationError, CompilationMessageSource, ErrorList}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object TestResourceAccess {

  def layer: ZLayer[FileIO, Throwable, Has[Service]] =
    ZLayer.fromManaged(ZManaged.accessManaged[FileIO] { env =>
      val fileIO = env.get

      for {
        libDir <- ZManaged.fromEffect(
          fileIO.getEnv("ARGON_LIB_DIR")
            .get
            .mapError(_ => new RuntimeException("ARGON_LIB_DIR was not set"))
        )

        liveResAccessEnv <- ResourceAccess.forFileIO.build
        liveResAccess = liveResAccessEnv.get
      } yield (new Service {

        override def getLibPath(name: String): UIO[Path] =
          Path.of(libDir, name, name + ".armodule")

        private def convertId(id: ResourceIndicator): IO[ErrorList, ResourceIndicator] = id match {
          case LibraryResourceIndicator(name) => getLibPath(name).map(PathResourceIndicator.apply)
          case id => IO.fail(NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(new FileNotFoundException()))))
        }

        override def writeToResource[X](id: ResourceIndicator)(data: Source[Comp, Chunk[Byte], X]): Comp[X] =
          data.foldLeftM(()) { (_, _) => IO.unit }.map { case (_, x) => x }

        override def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit] =
          liveResAccess.zipFromEntries(entries)

        override type ZipReader = liveResAccess.ZipReader

        override def getZipReader(id: ResourceIndicator): Managed[ErrorList, ZipReader] =
          ZManaged.fromEffect(convertId(id)).flatMap { id2 => liveResAccess.getZipReader(id2) }

        override def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit] =
          liveResAccess.zipEntryStream(zip, name)

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A] =
          liveResAccess.deserializeProtocolBuffer(companion)(data)

        override def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit] =
          liveResAccess.serializeProtocolBuffer(message)
      } : Service)
    })

  trait Service extends ResourceAccess.Service {
    def getLibPath(name: String): UIO[Path]
  }

}
