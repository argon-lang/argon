package com.mi3software.argon.build

import java.io.File

import scalaz.effect.IO

trait CompilationOutput {
  def writeToFile(outputFile: File): IO[Unit]
  def toByteArray: IO[Array[Byte]]
}
