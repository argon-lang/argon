package dev.argon.io

import java.io
import java.io.{ File, IOException }
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import cats._
import zio._
import zio.interop._

object FileOperations {

  implicit val fileShow: Show[File] = new Show[File] {
    override def show(f: File): String = f.toString
  }

  def fileFromName(fileName: String): UIO[io.File] =
    IO.effectTotal { new io.File(fileName) }

}
