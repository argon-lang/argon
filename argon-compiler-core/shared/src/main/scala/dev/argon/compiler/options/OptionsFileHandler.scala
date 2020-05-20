package dev.argon.compiler.options

import java.io.IOException

import dev.argon.io.Path.PathExtensions
import dev.argon.io.fileio.FileIO
import dev.argon.io.{FilenameManip, Path}
import zio._
import zio.stream.ZStream


trait OptionsFileHandler[-R, +E, -IOld, +I] {
  def loadSingleFile(file: IOld): ZIO[R, E, I]
  def loadGlobList(files: List[IOld]): ZIO[R, E, List[I]]
}

object OptionsFileHandler {

  def nothingFileHandler[R, E]: OptionsFileHandler[R, E, Nothing, Nothing] = new OptionsFileHandler[R, E, Nothing, Nothing] {
    override def loadSingleFile(file: Nothing): ZIO[R, E, Nothing] = file
    override def loadGlobList(files: List[Nothing]): ZIO[R, E, List[Nothing]] = IO.succeed(files)
  }

}