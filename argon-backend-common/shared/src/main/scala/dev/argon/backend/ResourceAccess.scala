package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.{Path, ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object ResourceAccess {

  def forFileIO[P: Path : Tag]: ZLayer[FileIO[P] with FileIOLite, Nothing, ResourceAccess[PathResourceIndicator[P]]] =
    PathResourceIndicator.pathResourceReader ++ ResourceWriter.forFileIO

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceAccess[Nothing]] =
    ResourceReader.forNothing ++ ResourceWriter.forNothing

}
