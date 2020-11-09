package dev.argon.compiler.loaders

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.io.{Path, StreamableMessage, ZipFileReader}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream._

object ResourceReader {

  trait Service[I <: ResourceIndicator] extends ResourceReaderPlatformSpecific.Service[I] {
    def readFile(id: I): Stream[CompilationError, Byte]
    def readTextFile(id: I): Stream[CompilationError, Char]
    def readTextFileAsString(id: I): Comp[String]
    def getZipReader(id: I): Managed[CompilationError, ZipFileReader[Any, CompilationError]]

    def deserializeProtocolBuffer[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Stream[CompilationError, Byte]): Comp[A]

    def deserializeProtocolBufferStream[R, A >: Null <: AnyRef](companion: StreamableMessage[A])(data: ZStream[R, CompilationError, Byte]): ZStream[R, CompilationError, A]
  }

  trait ServiceCommon[I <: ResourceIndicator] extends Service[I] {

    protected val fileIOLite: FileIOLite.Service

    override def deserializeProtocolBuffer[A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: Stream[CompilationError, Byte]): Comp[A] =
      fileIOLite.deserializeProtocolBuffer(Compilation.unwrapThrowableCause)(companion)(data)

    override def deserializeProtocolBufferStream[R, A >: Null <: AnyRef](companion: StreamableMessage[A])(data: ZStream[R, CompilationError, Byte]): ZStream[R, CompilationError, A] =
      fileIOLite.deserializeProtocolBufferStream[R, CompilationError, A](Compilation.unwrapThrowableCause)(companion)(data)
  }

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceReader[Nothing]] =
    ZLayer.fromFunction { env =>
      new ServiceCommon[Nothing] with ResourceReaderPlatformSpecific.ForNothingService {
        override protected val fileIOLite: FileIOLite.Service = env.get

        override def readFile(id: Nothing): Stream[CompilationError, Byte] = id

        override def readTextFile(id: Nothing): Stream[CompilationError, Char] = id

        override def readTextFileAsString(id: Nothing): Comp[String] = id

        override def getZipReader(id: Nothing): Managed[CompilationError, ZipFileReader[Any, CompilationError]] = id

      }
    }

}
