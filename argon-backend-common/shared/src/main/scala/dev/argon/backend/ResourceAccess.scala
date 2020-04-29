package dev.argon.backend

import java.io.IOException

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.{Path, ZipEntryInfo, ZipFileReader}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

object ResourceAccess {

  private[backend] trait ResourceAccessCommon {
    protected def ioExceptionToError(ex: IOException): NonEmptyList[CompilationError] =
      NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))
  }


  def forFileIO: ZLayer[FileIO, Nothing, ResourceAccess[PathResourceIndicator]] =
    ResourceReader.forFileIO ++ ResourceWriter.forFileIO

  def forNothing: ZLayer[FileIO, Nothing, ResourceAccess[Nothing]] =
    ResourceReader.forNothing ++ ResourceWriter.forNothing

}
