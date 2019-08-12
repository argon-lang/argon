package dev.argon.io

import dev.argon.stream._

import java.io._
import java.nio.file.Path

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
    def fileOutputTransformation[E](errorHandler: IOException => E)(path: Path): Managed[E, StreamTransformation[ZIO, Any, E, Byte, Unit, Nothing, Unit]]

    def isDirectory(path: Path): IO[IOException, Boolean]
    def listDirectory(path: Path): Stream[IOException, Path]

    def zipEntries[R, E](errorHandler: IOException => E)(entries: ArStream[ZIO, R, E, ZipEntryInfo[ZIO, R, E]]): ArStream[ZIO, R, E, Byte]
    def openZipFile[E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[E]]

    def protocolBufferSink[E, A <: GeneratedMessage with Message[A]](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A]): StreamTransformation[ZIO, Any, E, Byte, Unit, Nothing, A]
    def protocolBufferStream[E](errorHandler: IOException => E)(message: GeneratedMessage): ArStream[ZIO, Any, E, Byte]
  }

}
