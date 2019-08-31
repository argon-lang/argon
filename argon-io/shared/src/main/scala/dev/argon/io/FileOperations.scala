package dev.argon.io

import java.io
import java.nio.file.Path

import cats._
import zio._

object FileOperations {

  implicit val pathShow: Show[Path] = FilenameManip.pathToString _

  def pathFromName(fileName: String): UIO[Path] =
    IO.effectTotal { Path.of(fileName) }

}
