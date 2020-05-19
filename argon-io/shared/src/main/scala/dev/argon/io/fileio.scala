package dev.argon.io

import dev.argon.stream._
import java.io._

import dev.argon.stream.builder.Source
import zio._
import zio.stream._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

object fileio {
  type FileIOLite = Has[FileIOLite.Service]
  type FileIO[P] = Has[FileIO.Service[P]]

  object FileIO {

    trait Service[P] {
      def getAbsolutePath(path: P): IO[IOException, P]
      def ensureParentDirectory(path: P): IO[IOException, Unit]

      def readAllText(path: P): IO[IOException, String]
      def readText[E](errorHandler: IOException => E)(path: P): Stream[E, Char]
      def writeToFile[R, E](errorHandler: IOException => E)(path: P)(data: ZStream[R, E, Chunk[Byte]]): ZIO[R, E, Unit]

      def isDirectory(path: P): IO[IOException, Boolean]
      def listDirectory(path: P): Stream[IOException, P]

      def openZipFile[R, E](errorHandler: IOException => E)(path: P): Managed[E, ZipFileReader[R, E]]
    }

  }

  object FileIOLite {
    trait Service {
      def zipEntries[R, E](errorHandler: IOException => E)(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Chunk[Byte]]

      def deserializeProtocolBuffer[R, E, A <: GeneratedMessage]
      (errorHandler: IOException => E)
      (companion: GeneratedMessageCompanion[A])
      (data: ZStream[R, E, Chunk[Byte]])
      : ZIO[R, E, A]

      def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): ZStream[R, E, Chunk[Byte]]
    }
  }

}
