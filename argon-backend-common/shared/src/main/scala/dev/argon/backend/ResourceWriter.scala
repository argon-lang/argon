package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import scalapb.GeneratedMessage
import zio._
import zio.stream._

object ResourceWriter {

  trait Service[I <: ResourceIndicator] {
    def writeToResource(id: I)(data: Stream[ErrorList, Byte]): Comp[Unit]
    def zipFromEntries(entries: Stream[ErrorList, ZipEntryInfo[Any, ErrorList]]): Stream[ErrorList, Byte]

    def serializeProtocolBuffer(message: GeneratedMessage): Stream[ErrorList, Byte]
  }

  private trait ServiceCommon[I <: ResourceIndicator] extends Service[I] {
    protected val fileIOLite: FileIOLite.Service

    protected def ioExceptionToError(ex: IOException): NonEmptyList[CompilationError] =
      NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))


    override def zipFromEntries(entries: Stream[ErrorList, ZipEntryInfo[Any, ErrorList]]): Stream[ErrorList, Byte] =
      fileIOLite.zipEntries(ioExceptionToError)(entries)

    override def serializeProtocolBuffer(message: GeneratedMessage): Stream[ErrorList, Byte] =
      fileIOLite.serializeProtocolBuffer(ioExceptionToError)(message)

  }

  def forFileIO[P: Path : Tag]: ZLayer[FileIO[P] with FileIOLite, Nothing, ResourceWriter[PathResourceIndicator[P]]] =
    ZLayer.fromFunction { prevLayer =>
      val fileIO = prevLayer.get[FileIO.Service[P]]
      new ServiceCommon[PathResourceIndicator[P]] {
        override protected val fileIOLite: FileIOLite.Service = prevLayer.get[FileIOLite.Service]

        override def writeToResource(id: PathResourceIndicator[P])(data: Stream[ErrorList, Byte]): Comp[Unit] =
          fileIO.ensureParentDirectory(id.path).mapError(ioExceptionToError) *>
            fileIO.writeToFile(ioExceptionToError)(id.path)(data)
      }
    }

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceWriter[Nothing]] =
    ZLayer.fromFunction { prevLayer =>
      new ServiceCommon[Nothing] {
        override protected val fileIOLite: FileIOLite.Service = prevLayer.get

        override def writeToResource(id: Nothing)(data: Stream[ErrorList, Byte]): Comp[Unit] = id
      }
    }

}
