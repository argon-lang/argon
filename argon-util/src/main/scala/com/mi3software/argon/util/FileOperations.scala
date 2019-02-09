package com.mi3software.argon.util

import java.io
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}

import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._
import MonadErrorExtensions._

object FileOperations {

  implicit val fileShow: Show[java.io.File] = new Show[java.io.File] {
    override def shows(f: File): String = f.toString
  }

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]
  type ZIOThrowable[F[_]] = ZIO[Lambda[(E, A) => F[A]]]

  def fileInputStream[F[_, _]: MonadErrorThrowable : ZIO, T](file: io.File)(f: io.FileInputStream => F[Throwable, T]): F[Throwable, T] =
    ZIO[F].liftZIO(IO.syncThrowable { new io.FileInputStream(file) })
      .flatMap { stream =>
        f(stream).closing(stream)
      }

  def fileOutputStream[T](file: io.File)(f: io.FileOutputStream => IO[Throwable, T]): IO[Throwable, T] =
    IO.syncThrowable { new io.FileOutputStream(file) }
      .flatMap { stream =>
        f(stream).closing(stream)
      }

  def createPrintWriter[F[_, _]: MonadErrorThrowable : ZIO, T](stream: io.OutputStream)(f: io.PrintWriter => F[Throwable, T]): F[Throwable, T] =
    ZIO[F].liftZIO(IO.syncThrowable { new io.PrintWriter(stream) })
      .flatMap { writer =>
        f(writer).closing(writer)
      }

  def filePrintWriter[T](file: io.File)(f: io.PrintWriter => IO[Throwable, T]): IO[Throwable, T] =
    fileOutputStream(file) { stream => createPrintWriter(stream)(f) }

  def readAllText(file: io.File): IO[Throwable, String] =
    IO.syncThrowable {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, StandardCharsets.UTF_8)
    }

  def createReader[F[_, _] : ZIO, T](stream: io.FileInputStream): F[Throwable, io.Reader] =
    ZIO[F].liftZIO(IO.syncThrowable { new io.InputStreamReader(stream) })

  def fileReader[F[_, _]: MonadErrorThrowable : ZIO, T](file: io.File)(f: io.Reader => F[Throwable, T]): F[Throwable, T] =
    fileInputStream(file) { stream => createReader(stream).flatMap(f) }

  def fileFromName(fileName: String): IO[Throwable, io.File] =
    IO.syncThrowable { new io.File(fileName) }

  def zipOutputStream[F2[_, _] : MonadErrorThrowable : ZIO, A](stream: io.OutputStream)(f: ZipOutputStream => F2[Throwable, A]): F2[Throwable, A] =
    ZIO[F2].liftZIO(IO.syncThrowable { new ZipOutputStream(stream) })
      .flatMap { zip => f(zip).closing(zip) }

  def createZipEntry[F2[_, _] : MonadErrorThrowable : ZIO, A](zip: ZipOutputStream, path: String)(f: ZipEntry => F2[Throwable, A]): F2[Throwable, A] =
    ZIO[F2].liftZIO(IO.syncThrowable {
      val entry = new ZipEntry(path)
      zip.putNextEntry(entry)
      entry
    })
      .flatMap { entry =>
        f(entry).ensuring(
          ZIO[F2].liftZIO(IO.syncThrowable { zip.closeEntry() })
        )
      }

}
