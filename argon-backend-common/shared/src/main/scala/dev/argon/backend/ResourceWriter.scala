package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{ZipEntryInfo}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.GeneratedMessage
import zio._

object ResourceWriter {

  trait Service[I <: ResourceIndicator] {
    def writeToResource[X](id: I)(data: Source[Comp, Chunk[Byte], X]): Comp[X]
    def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit]

    def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit]
  }

  private trait ServiceCommon[I <: ResourceIndicator] extends Service[I] with ResourceAccess.ResourceAccessCommon {
    protected val fileIO: FileIO.Service


    override def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit] =
      fileIO.zipEntries(ioExceptionToError)(entries)

    override def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit] =
      fileIO.serializeProtocolBuffer(ioExceptionToError)(message)

  }

  def forFileIO: ZLayer[FileIO, Nothing, ResourceWriter[PathResourceIndicator]] =
    ZLayer.fromFunction { prevLayer =>
      new ServiceCommon[PathResourceIndicator] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIO: FileIO.Service = prevLayer.get

        override def writeToResource[X](id: PathResourceIndicator)(data: Source[Comp, Chunk[Byte], X]): Comp[X] =
          fileIO.writeToFile(ioExceptionToError)(id.path)(data)
      }
    }

  def forNothing: ZLayer[FileIO, Nothing, ResourceWriter[Nothing]] =
    ZLayer.fromFunction { prevLayer =>
      new ServiceCommon[Nothing] with ResourceAccess.ResourceAccessCommon {
        override protected val fileIO: FileIO.Service = prevLayer.get

        override def writeToResource[X](id: Nothing)(data: Source[Comp, Chunk[Byte], X]): Comp[X] = id
      }
    }

}
