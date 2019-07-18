package dev.argon.util

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


  def readAllText(file: io.File): IO[IOException, String] =
    IO.effect {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, StandardCharsets.UTF_8)
    }
      .refineOrDie { case e: IOException => e }

  def fileFromName(fileName: String): UIO[io.File] =
    IO.effectTotal { new io.File(fileName) }

}
