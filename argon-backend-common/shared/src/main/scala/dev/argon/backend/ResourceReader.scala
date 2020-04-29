package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object ResourceReader {

  trait Service[I <: ResourceIndicator] {
    type ZipReader
    def getZipReader(id: I): Managed[ErrorList, ZipReader]
    def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit]

    def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A]
  }

  private trait ServiceCommon[I <: ResourceIndicator] extends Service[I] with ResourceAccess.ResourceAccessCommon {

    protected val fileIO: FileIO.Service

    override type ZipReader = ZipFileReader[Comp]

    override def zipEntryStream(zip: ZipReader, name: String): Source[Comp, Chunk[Byte], Unit] =
      zip.getEntryStream(name)

    override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A] =
      fileIO.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)

  }

  def forFileIO: ZLayer[FileIO, Nothing, ResourceReader[PathResourceIndicator]] =
    ZLayer.fromFunction { prevLayer =>
      new ServiceCommon[PathResourceIndicator] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIO: FileIO.Service = prevLayer.get

        override def getZipReader(id: PathResourceIndicator): Managed[ErrorList, ZipFileReader[Comp]] =
          fileIO.openZipFile(ioExceptionToError)(id.path)
      }
    }

  def forNothing: ZLayer[FileIO, Nothing, ResourceReader[Nothing]] =
    ZLayer.fromFunction { env =>
      new ServiceCommon[Nothing] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIO: FileIO.Service = env.get

        override def getZipReader(id: Nothing): Managed[ErrorList, ZipFileReader[Comp]] = id
      }
    }

}
