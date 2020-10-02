package dev.argon.io

import dev.argon.stream._
import java.io._

import zio._
import zio.stream._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

object fileio {
  type FileIOLite = Has[FileIOLite.Service]
  type FileIO[P] = Has[FileIO.Service[P]]

  object FileIO {

    trait Service[P] extends FileIOServicePlatformSpecific[P] {
      def getAbsolutePath(path: P): IO[Throwable, P]
      def ensureParentDirectory(path: P): IO[Throwable, Unit]

      def readFile[E](errorHandler: Throwable => Cause[E])(path: P): Stream[E, Byte]
      def readAllText(path: P): IO[Throwable, String]
      def readText[E](errorHandler: Throwable => Cause[E])(path: P): Stream[E, Char]
      def writeToFile[R, E](errorHandler: Throwable => Cause[E])(path: P)(data: ZStream[R, E, Byte]): ZIO[R, E, Unit]

      def isDirectory(path: P): IO[Throwable, Boolean]
      def listDirectory(path: P): Stream[Throwable, P]

      def openZipFile[R, E](errorHandler: Throwable => Cause[E])(path: P): Managed[E, ZipFileReader[R, E]]
    }

  }

  object FileIOLite {
    trait Service {
      def zipEntries[R, E <: Throwable](errorHandler: Throwable => Cause[E])(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Byte]

      def deserializeProtocolBuffer[R, E <: Throwable, A <: GeneratedMessage]
      (errorHandler: Throwable => Cause[E])
      (companion: GeneratedMessageCompanion[A])
      (data: ZStream[R, E, Byte])
      : ZIO[R, E, A]

      def serializeProtocolBuffer[E <: Throwable](errorHandler: Throwable => Cause[E])(message: GeneratedMessage): Stream[E, Byte]
    }
  }

}
