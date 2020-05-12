package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object ResourceReader {

  trait Service[I <: ResourceIndicator] {
    def getZipReader(id: I): Managed[ErrorList, ZipFileReader[Any, ErrorList]]

    def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Any, ErrorList, Chunk[Byte], Unit]): Comp[A]
  }

  private trait ServiceCommon[I <: ResourceIndicator] extends Service[I] with ResourceAccess.ResourceAccessCommon {

    protected val fileIOLite: FileIOLite.Service

    override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Source[Any, ErrorList, Chunk[Byte], Unit]): Comp[A] =
      fileIOLite.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)

  }

  def forFileIO[P: Path: Tagged]: ZLayer[FileIO[P] with FileIOLite, Nothing, ResourceReader[PathResourceIndicator[P]]] =
    ZLayer.fromFunction { prevLayer =>
      val fileIO = prevLayer.get[FileIO.Service[P]]
      new ServiceCommon[PathResourceIndicator[P]] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIOLite: FileIOLite.Service = prevLayer.get[FileIOLite.Service]

        override def getZipReader(id: PathResourceIndicator[P]): Managed[ErrorList, ZipFileReader[Any, ErrorList]] =
          fileIO.openZipFile(ioExceptionToError)(id.path)
      }
    }

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceReader[Nothing]] =
    ZLayer.fromFunction { env =>
      new ServiceCommon[Nothing] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIOLite: FileIOLite.Service = env.get

        override def getZipReader(id: Nothing): Managed[ErrorList, ZipFileReader[Any, ErrorList]] = id
      }
    }

}
