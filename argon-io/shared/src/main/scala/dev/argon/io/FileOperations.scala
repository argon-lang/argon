package dev.argon.io

import java.io
import java.nio.file.Path

import cats._
import zio._

object FileOperations {

  implicit val pathShow: Show[Path] = FilenameManip.pathToString _

  def fileFromName(fileName: String): UIO[io.File] =
    IO.effectTotal { new io.File(fileName) }

}
