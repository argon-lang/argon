package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object ResourceAccess {

  private[backend] trait ResourceAccessCommon {
    protected def ioExceptionToError(ex: IOException): NonEmptyList[CompilationError] =
      NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))
  }


  def forFileIO[P: Path : Tagged]: ZLayer[FileIO[P] with FileIOLite, Nothing, ResourceAccess[PathResourceIndicator[P]]] =
    ResourceReader.forFileIO ++ ResourceWriter.forFileIO

  def forNothing: ZLayer[FileIOLite, Nothing, ResourceAccess[Nothing]] =
    ResourceReader.forNothing ++ ResourceWriter.forNothing

}
