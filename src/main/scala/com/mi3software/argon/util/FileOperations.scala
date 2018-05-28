package com.mi3software.argon.util

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scalaz.effect.IO

object FileOperations {

  def fileInputStream[T](file: File)(f: FileInputStream => IO[T]): IO[T] =
    IO { new FileInputStream(file) }
      .flatMap { stream =>
        f(stream).ensuring(IO { stream.close() })
      }

  def readAllText(file: File): IO[String] =
    IO {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, StandardCharsets.UTF_8)
    }

  def fileFromName(fileName: String): IO[File] =
    IO { new File(fileName) }

}
