package com.mi3software.argon.backend

import java.io.File

import scalaz.effect.IO

trait CompilationResult {
  def writeToFile(outputFile: File): IO[Unit]
}
