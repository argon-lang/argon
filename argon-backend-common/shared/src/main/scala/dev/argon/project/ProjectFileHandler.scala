package dev.argon.project

import java.io.IOException

import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.io.{FilenameManip, Path}
import dev.argon.io.Path.PathExtensions
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import zio.stream.ZStream
import zio._
import zio.interop.catz.core._


trait ProjectFileHandler[-R, +E, -IOld, +I] {
  def loadSingleFile(file: IOld): ZIO[R, E, I]
  def loadGlobList(files: List[IOld]): ZIO[R, E, List[I]]
}

object ProjectFileHandler {

  def fileHandlerPath[P : Path: Tagged](dir: P): ProjectFileHandler[FileIO[P], IOException, String, PathResourceIndicator[P]] = new ProjectFileHandler[FileIO[P], IOException, String, PathResourceIndicator[P]] {

    override def loadSingleFile(file: String): ZIO[FileIO[P], IOException, PathResourceIndicator[P]] =
      Path.of[P](file).map(dir.resolve).map(PathResourceIndicator(_))

    override def loadGlobList(files: List[String]): ZIO[FileIO[P], IOException, List[PathResourceIndicator[P]]] =
      ZStream.fromIterable(files)
        .mapM(Path.of(_))
        .flatMap { path =>
          FilenameManip.findGlob(dir, path)
        }
        .map(PathResourceIndicator(_))
        .runCollect

  }

  def nothingFileHandler[R, E]: ProjectFileHandler[R, E, Nothing, Nothing] = new ProjectFileHandler[R, E, Nothing, Nothing] {
    override def loadSingleFile(file: Nothing): ZIO[R, E, Nothing] = file
    override def loadGlobList(files: List[Nothing]): ZIO[R, E, List[Nothing]] = IO.succeed(files)
  }

}