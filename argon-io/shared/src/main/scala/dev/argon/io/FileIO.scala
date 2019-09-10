package dev.argon.io

import dev.argon.stream._
import java.io._
import java.nio.file.Path

import dev.argon.stream.builder.Source
import zio._
import zio.stream._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

trait FileIO {
  val fileIO: FileIO.Service
}

object FileIO {

  trait Service {
    def getAbsolutePath(path: Path): IO[IOException, Path]

    def readAllText(path: Path): IO[IOException, String]
    def readText[E](errorHandler: IOException => E)(path: Path): Stream[E, Char]
    def writeToFile[R, E, X](errorHandler: IOException => E)(path: Path)(data: Source[ZIO[R, E, *], Chunk[Byte], X]): ZIO[R, E, X]

    def isDirectory(path: Path): IO[IOException, Boolean]
    def listDirectory(path: Path): Stream[IOException, Path]

    def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, ?]], Unit]): Source[ZIO[R, E, ?], Chunk[Byte], Unit]
    def openZipFile[R, E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[ZIO[R, E, ?]]]

    def deserializeProtocolBuffer[R, E, A <: GeneratedMessage with Message[A]]
    (errorHandler: IOException => E)
    (companion: GeneratedMessageCompanion[A])
    (data: Source[ZIO[R, E, *], Chunk[Byte], Unit])
    : ZIO[R, E, A]

    def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[ZIO[R, E, ?], Chunk[Byte], Unit]
  }

}
