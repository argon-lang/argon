package com.mi3software.argon.build

import java.io.File

import scalaz.effect.IO

trait CompilationResult {
  def writeToFile(outputFile: File): IO[Unit]
}
