package dev.argon.compiler.loaders

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.io.{Path, ZipFileReader}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream._

object ResourceReader {

  trait Service[I <: ResourceIndicator] extends ResourceReaderPlatformSpecific.Service[I] {
    def readFile(id: I): Stream[ErrorList, Byte]
    def readTextFile(id: I): Stream[ErrorList, Char]
    def readTextFileAsString(id: I): Comp[String]
    def getZipReader(id: I): Managed[ErrorList, ZipFileReader[Any, ErrorList]]

    def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Stream[ErrorList, Byte]): Comp[A]
  }

  trait ServiceCommon[I <: ResourceIndicator] extends Service[I] {

    protected val fileIOLite: FileIOLite.Service

    protected def ioExceptionToError(ex: IOException): ErrorList =
      NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

    override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Stream[ErrorList, Byte]): Comp[A] =
      fileIOLite.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)

  }

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceReader[Nothing]] =
    ZLayer.fromFunction { env =>
      new ServiceCommon[Nothing] with ResourceReaderPlatformSpecific.ForNothingService {
        override protected val fileIOLite: FileIOLite.Service = env.get

        override def readFile(id: Nothing): Stream[ErrorList, Byte] = id

        override def readTextFile(id: Nothing): Stream[ErrorList, Char] = id

        override def readTextFileAsString(id: Nothing): Comp[String] = id

        override def getZipReader(id: Nothing): Managed[ErrorList, ZipFileReader[Any, ErrorList]] = id
      }
    }

}
