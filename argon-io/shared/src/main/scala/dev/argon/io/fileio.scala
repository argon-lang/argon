package dev.argon.io

import dev.argon.stream._
import java.io._

import zio._
import zio.stream._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

object fileio {
  type FileIO = Has[FileIO.Service]
  type ZipRead = Has[ZipRead.Service]

  object FileIO {

    trait Service {
      def readFile(path: String): Stream[Throwable, Byte]
      def readAllText(path: String): IO[Throwable, String]
      def readText(path: String): Stream[Throwable, Char]
      def writeToFile[R](path: String)(data: ZStream[R, Throwable, Byte]): ZIO[R, Throwable, Unit]

      def isDirectory(path: String): IO[Throwable, Boolean]
      def listDirectory(path: String): Stream[Throwable, String]

    }

  }

  object ZipRead {
    trait Service {
      def openZipFile(path: String): Managed[Throwable, ZipFileReader[Throwable]]
    }
  }

}
