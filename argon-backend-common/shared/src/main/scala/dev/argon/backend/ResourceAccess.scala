package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object ResourceAccess {

  def forFileIO: ZLayer[FileIO, Nothing, ResourceAccess] =
    ZLayer.fromFunction { prevLayer =>
      new Service {
        private val fileIO = prevLayer.get

        private def ioExceptionToError(ex: IOException): NonEmptyList[CompilationError] =
          NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

        private def acceptPathId[A](id: ResourceIndicator)(f: Path => Comp[A]): Comp[A] =
          id match {
            case PathResourceIndicator(path) => f(path)
            case _ => IO.fail(ioExceptionToError(new IOException("Resource indicator was expected to be a path.")))
          }



        override def writeToResource[X](id: ResourceIndicator)(data: Source[Comp, Chunk[Byte], X]): Comp[X] =
          acceptPathId(id) { path =>
            fileIO.writeToFile(ioExceptionToError)(path)(data)
          }

        override def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit] =
          fileIO.zipEntries(ioExceptionToError)(entries)

        override type ZipReader = ZipFileReader[Comp]

        override def getZipReader(id: ResourceIndicator): Managed[ErrorList, ZipFileReader[Comp]] =
          ZManaged.unwrap(acceptPathId(id) { path =>
            IO.succeed(fileIO.openZipFile(ioExceptionToError)(path))
          })


        override def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit] =
          zip.getEntryStream(name)


        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A] =
          fileIO.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)

        override def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit] =
          fileIO.serializeProtocolBuffer(ioExceptionToError)(message)
      }
    }

  trait Service {
    def writeToResource[X](id: ResourceIndicator)(data: Source[Comp, Chunk[Byte], X]): Comp[X]
    def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit]

    type ZipReader
    def getZipReader(id: ResourceIndicator): Managed[ErrorList, ZipReader]
    def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit]

    def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A]
    def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit]
  }

}
