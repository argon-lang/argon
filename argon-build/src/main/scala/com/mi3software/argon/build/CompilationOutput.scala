package com.mi3software.argon.build

import java.io.{OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.ResourceAccess
import scalaz._
import Scalaz._
import com.mi3software.argon.util.{FileOperations, IOHelpers}
import org.apache.commons.io.output.NullOutputStream
import scalaz.Leibniz.===
import scalaz.zio.{IO, ZIO}

trait CompilationOutput[F[+_]] {
  type MonadErrorThrowable[A[_, _]] = MonadError[A[Throwable, ?], Throwable]

  def write[F2[_, _]: MonadErrorThrowable: ZIO](stream: OutputStream)(implicit ev: F[Unit] === F2[Throwable, Unit]): F2[Throwable, Unit]

  def runDiscard[F2[_, _]: MonadErrorThrowable: ZIO](implicit ev: F[Unit] === F2[Throwable, Unit]): F2[Throwable, Unit] =
    ZIO[F2].liftZIO(IO.syncThrowable { new NullOutputStream() }).flatMap(write[F2])
}

trait CompilationOutputText[F[+_]] extends CompilationOutput[F] {

  override def write[F2[_, _] : MonadErrorThrowable : ZIO](stream: OutputStream)(implicit ev: F[Unit] === F2[Throwable, Unit]): F2[Throwable, Unit] =
    FileOperations.createPrintWriter(stream) { writer =>
      ZIO[F2].liftZIO(IO.syncThrowable { writeText(writer) })
    }

  def writeText(writer: PrintWriter): Unit

}
