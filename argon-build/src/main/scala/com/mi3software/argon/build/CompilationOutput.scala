package com.mi3software.argon.build

import java.io.{OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.ResourceAccess
import scalaz._
import Scalaz._

trait CompilationOutput {
  def write(stream: OutputStream): Unit
}

trait CompilationOutputText extends CompilationOutput {

  override def write(stream: OutputStream): Unit = {
    val writer = new PrintWriter(stream)
    try {
      writeText(writer)
    }
    finally writer.close()
  }

  def writeText(writer: PrintWriter): Unit

}
