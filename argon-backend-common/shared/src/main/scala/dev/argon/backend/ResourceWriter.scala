package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.GeneratedMessage
import zio._

object ResourceWriter {

  trait Service[I <: ResourceIndicator] {
    def writeToResource[X](id: I)(data: Source[Any, ErrorList, Chunk[Byte], X]): Comp[X]
    def zipFromEntries(entries: Source[Any, ErrorList, ZipEntryInfo[Any, ErrorList], Unit]): Source[Any, ErrorList, Chunk[Byte], Unit]

    def serializeProtocolBuffer(message: GeneratedMessage): Source[Any, ErrorList, Chunk[Byte], Unit]
  }

  private trait ServiceCommon[I <: ResourceIndicator] extends Service[I] with ResourceAccess.ResourceAccessCommon {
    protected val fileIOLite: FileIOLite.Service


    override def zipFromEntries(entries: Source[Any, ErrorList, ZipEntryInfo[Any, ErrorList], Unit]): Source[Any, ErrorList, Chunk[Byte], Unit] =
      fileIOLite.zipEntries(ioExceptionToError)(entries)

    override def serializeProtocolBuffer(message: GeneratedMessage): Source[Any, ErrorList, Chunk[Byte], Unit] =
      fileIOLite.serializeProtocolBuffer(ioExceptionToError)(message)

  }

  def forFileIO[P: Path : Tagged]: ZLayer[FileIO[P] with FileIOLite, Nothing, ResourceWriter[PathResourceIndicator[P]]] =
    ZLayer.fromFunction { prevLayer =>
      val fileIO = prevLayer.get[FileIO.Service[P]]
      new ServiceCommon[PathResourceIndicator[P]] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIOLite: FileIOLite.Service = prevLayer.get[FileIOLite.Service]

        override def writeToResource[X](id: PathResourceIndicator[P])(data: Source[Any, ErrorList, Chunk[Byte], X]): Comp[X] =
          fileIO.writeToFile(ioExceptionToError)(id.path)(data)
      }
    }

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceWriter[Nothing]] =
    ZLayer.fromFunction { prevLayer =>
      new ServiceCommon[Nothing] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIOLite: FileIOLite.Service = prevLayer.get

        override def writeToResource[X](id: Nothing)(data: Source[Any, ErrorList, Chunk[Byte], X]): Comp[X] = id
      }
    }

}
