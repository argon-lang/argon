package dev.argon.util

import java.io
import java.io.{ File, IOException }
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import cats._
import scalaz.zio._
import scalaz.zio.interop._

object FileOperations {

  implicit val fileShow: Show[File] = new Show[File] {
    override def show(f: File): String = f.toString
  }


  def fileInputStream[E >: IOException, T](file: io.File)(f: io.FileInputStream => IO[E, T]): IO[E, T] =
    IO.effect { new io.FileInputStream(file) }
      .refineOrDie { case e: IOException => e }
      .bracketAuto(f)

  def fileOutputStream[E >: IOException, T](file: io.File)(f: io.FileOutputStream => IO[E, T]): IO[E, T] =
    IO.effect { new io.FileOutputStream(file) }
      .refineOrDie { case e: IOException => e }
      .bracketAuto(f)

  def createPrintWriter[E >: IOException, T](stream: io.OutputStream)(f: io.PrintWriter => IO[E, T]): IO[E, T] =
    IO.effect { new io.PrintWriter(stream) }
      .refineOrDie { case e: IOException => e }
      .bracketAuto(f)

  def filePrintWriter[E >: IOException, T](file: io.File)(f: io.PrintWriter => IO[E, T]): IO[E, T] =
    fileOutputStream[E, T](file) { stream => createPrintWriter[E, T](stream)(f) }

  def readAllText(file: io.File): IO[IOException, String] =
    IO.effect {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, StandardCharsets.UTF_8)
    }
      .refineOrDie { case e: IOException => e }

  def createReader[T](stream: io.FileInputStream): UIO[io.Reader] =
    IO.effectTotal { new io.InputStreamReader(stream) }

  def fileReader[E >: IOException, T](file: io.File)(f: io.Reader => IO[E, T]): IO[E, T] =
    fileInputStream[E, T](file) { stream => createReader(stream).flatMap(f) }

  def fileFromName(fileName: String): UIO[io.File] =
    IO.effectTotal { new io.File(fileName) }

  def zipOutputStream[E >: IOException, A](stream: io.OutputStream)(f: ZipOutputStream => IO[E, A]): IO[E, A] =
    IO.effectTotal { new ZipOutputStream(stream) }.bracketAuto(f)

  def createZipEntry[E >: IOException, A](zip: ZipOutputStream, path: String)(f: ZipEntry => IO[E, A]): IO[E, A] =
    IO.effect {
      val entry = new ZipEntry(path)
      zip.putNextEntry(entry)
      entry
    }
      .refineOrDie { case e: IOException => e }
      .bracket(_ =>
        IO.effectTotal { zip.closeEntry() }
      )(f)

  def createZipFile[E >: IOException, A](file: File)(f: ZipFile => IO[E, A]): IO[E, A] =
    IO.effect { new ZipFile(file) }
      .refineOrDie { case e: IOException => e }
      .bracketAuto(f)

  def getZipEntryStream[E >: IOException, A](zip: ZipFile, name: String)(f: io.InputStream => IO[E, A]): IO[E, A] =
    IO.effect { zip.getInputStream(zip.getEntry(name)) }
      .refineOrDie { case e: IOException => e }
      .bracketAuto(f)
}
