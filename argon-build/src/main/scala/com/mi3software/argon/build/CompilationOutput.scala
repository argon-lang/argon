package com.mi3software.argon.build

import java.io.{OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.ResourceAccess
import scalaz._
import Scalaz._
import com.mi3software.argon.util.FileOperations
import org.apache.commons.io.output.NullOutputStream
import scalaz.Leibniz.===
import scalaz.zio.{IO, ZIO}

trait CompilationOutput[F[+_], I] {
  type MonadErrorThrowable[A[_, _]] = MonadError[A[Throwable, ?], Throwable]

  def write(implicit resourceAccess: ResourceAccess[F, I]): F[Unit]

}

trait CompilationOutputText[F[+_], I] extends CompilationOutput[F, I] {


  override def write(implicit resourceAccess: ResourceAccess[F, I]): F[Unit] =
    resourceAccess.createPrintWriter(outputResource)(writeText)

  def outputResource: I

  def writeText(writer: PrintWriter): Unit

}
