package dev.argon.compiler

import zio.*

trait ErrorLog {
  def logError(error: => CompilerError): UIO[Unit]
}

object ErrorLog {
  def logError(error: => CompilerError): URIO[ErrorLog, Unit] =
    ZIO.serviceWithZIO[ErrorLog](_.logError(error))
}
