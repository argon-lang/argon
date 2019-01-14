package com.mi3software.argon.util

import java.io
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scalaz._
import Scalaz._
import scalaz.effect.{IO, LiftIO}
import MonadErrorExtensions._
import IOHelpers.ioMonadError

object FileOperations {

  type MonadErrorThrowable[F[_]] = MonadError[F, Throwable]

  def fileInputStream[F[_]: MonadErrorThrowable : LiftIO, T](file: io.File)(f: io.FileInputStream => F[T]): F[T] =
    LiftIO[F].liftIO(IO { new io.FileInputStream(file) })
      .flatMap { stream =>
        f(stream).ensuring(LiftIO[F].liftIO(IO { stream.close() }))
      }

  def fileOutputStream[T](file: io.File)(f: io.FileOutputStream => IO[T]): IO[T] =
    IO { new io.FileOutputStream(file) }
      .flatMap { stream =>
        f(stream).ensuring(IO { stream.close() })
      }

  def createPrintWriter[T](stream: io.FileOutputStream)(f: io.PrintWriter => IO[T]): IO[T] =
    IO { new io.PrintWriter(stream) }
      .flatMap { writer =>
        f(writer).ensuring(IO { writer.close() })
      }

  def filePrintWriter[T](file: io.File)(f: io.PrintWriter => IO[T]): IO[T] =
    fileOutputStream(file) { stream => createPrintWriter(stream)(f) }

  def readAllText(file: io.File): IO[String] =
    IO {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, StandardCharsets.UTF_8)
    }

  def createReader[F[_] : LiftIO, T](stream: io.FileInputStream): F[io.Reader] =
    LiftIO[F].liftIO(IO { new io.InputStreamReader(stream) })

  def fileReader[F[_]: MonadErrorThrowable : LiftIO, T](file: io.File)(f: io.Reader => F[T]): F[T] =
    fileInputStream(file) { stream => createReader(stream).flatMap(f) }

  def fileFromName(fileName: String): IO[io.File] =
    IO { new io.File(fileName) }

}
