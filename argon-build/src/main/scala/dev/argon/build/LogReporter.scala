package dev.argon.build

import zio.*
import dev.argon.compiler.*

trait LogReporter {
  def getErrors: UIO[Seq[CompilerError]]
  def reportLogs: UIO[Unit] =
    getErrors.flatMap { errors =>
      if errors.isEmpty then
        ZIO.unit
      else
        ZIO.foreachDiscard(errors) { e => Console.printLineError(e).orDie }
      end if
    }

  def failOnErrors: IO[BuildError, Unit] =
    getErrors.flatMap { errors =>
      if errors.isEmpty then
        ZIO.unit
      else
        ZIO.fail(BuildFailed(errors))
      end if
    }
}

object LogReporter {
  val live: ULayer[ErrorLog & LogReporter] =
    ZLayer.fromZIOEnvironment(
      for
        errors <- Ref.make(Seq.empty[CompilerError])
      yield ZEnvironment[ErrorLog, LogReporter](
        new ErrorLog {
          override def logError(error: => CompilerError): UIO[Unit] =
            errors.update(_ :+ error)
        },
        new LogReporter {
          override def getErrors: UIO[Seq[CompilerError]] =
            errors.get
        },
      )
    )
}
