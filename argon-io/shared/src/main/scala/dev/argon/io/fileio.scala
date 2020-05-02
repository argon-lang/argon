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

      def readAllText(path: P): IO[IOException, String]
      def readText[E](errorHandler: IOException => E)(path: P): Stream[E, Char]
      def writeToFile[R, E, X](errorHandler: IOException => E)(path: P)(data: Source[ZIO[R, E, *], Chunk[Byte], X]): ZIO[R, E, X]

      def isDirectory(path: P): IO[IOException, Boolean]
      def listDirectory(path: P): Stream[IOException, P]

      def openZipFile[R, E](errorHandler: IOException => E)(path: P): Managed[E, ZipFileReader[ZIO[R, E, *]]]
    }

  }

  object FileIOLite {
    trait Service {
      def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): Source[ZIO[R, E, *], Chunk[Byte], Unit]

      def deserializeProtocolBuffer[R, E, A <: GeneratedMessage]
      (errorHandler: IOException => E)
      (companion: GeneratedMessageCompanion[A])
      (data: Source[ZIO[R, E, *], Chunk[Byte], Unit])
      : ZIO[R, E, A]

      def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[ZIO[R, E, *], Chunk[Byte], Unit]
    }
  }

}
