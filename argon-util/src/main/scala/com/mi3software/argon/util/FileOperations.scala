package com.mi3software.argon.util

import java.io
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scalaz._
import Scalaz._
import scalaz.zio._

object FileOperations {

  implicit val fileShow: Show[java.io.File] = new Show[java.io.File] {
    override def shows(f: File): String = f.toString
  }

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]

  def fileInputStream[T](file: io.File)(f: io.FileInputStream => Task[T]): Task[T] =
    IO.effect { new io.FileInputStream(file) }.bracket(stream => IO.effectTotal { stream.close() })(f)

  def fileOutputStream[T](file: io.File)(f: io.FileOutputStream => Task[T]): Task[T] =
    IO.effect { new io.FileOutputStream(file) }.bracket(stream => IO.effectTotal { stream.close() })(f)

  def createPrintWriter[T](stream: io.OutputStream)(f: io.PrintWriter => Task[T]): Task[T] =
    IO.effect { new io.PrintWriter(stream) }.bracket(writer => IO.effectTotal { writer.close() })(f)

  def filePrintWriter[T](file: io.File)(f: io.PrintWriter => Task[T]): Task[T] =
    fileOutputStream(file) { stream => createPrintWriter(stream)(f) }

  def readAllText(file: io.File): Task[String] =
    IO.effect {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, StandardCharsets.UTF_8)
    }

  def createReader[T](stream: io.FileInputStream): Task[io.Reader] =
    IO.effect { new io.InputStreamReader(stream) }

  def fileReader[T](file: io.File)(f: io.Reader => Task[T]): Task[T] =
    fileInputStream(file) { stream => createReader(stream).flatMap(f) }

  def fileFromName(fileName: String): Task[io.File] =
    IO.effect { new io.File(fileName) }

  def zipOutputStream[A](stream: io.OutputStream)(f: ZipOutputStream => Task[A]): Task[A] =
    IO.effect { new ZipOutputStream(stream) }.bracket(stream => IO.effectTotal { stream.close() })(f)

  def createZipEntry[A](zip: ZipOutputStream, path: String)(f: ZipEntry => Task[A]): Task[A] =
    IO.effect {
      val entry = new ZipEntry(path)
      zip.putNextEntry(entry)
      entry
    }.bracket(_ => IO.effectTotal { zip.closeEntry() })(f)

  def createZipFile[A](file: File)(f: ZipFile => Task[A]): Task[A] =
    IO.effect { new ZipFile(file) }.bracket(f => IO.effectTotal { f.close() })(f)

  def getZipEntryStream[A](zip: ZipFile, name: String)(f: io.InputStream => Task[A]): Task[A] =
    IO.effect { zip.getInputStream(zip.getEntry(name)) }.bracket(f => IO.effectTotal { f.close() })(f)
}
