package dev.argon.platform

import zio.*
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}

abstract class PlatformApp[E] extends ZIOApp {

  override type Environment = Any
  type Error = E
  override def environmentTag: EnvironmentTag[Any] = summon[EnvironmentTag[Any]]

  final override def bootstrap: ZLayer[Any, Any, Any] =
    (Runtime.removeDefaultLoggers >+> consoleLogger(ConsoleLoggerConfig(
      format = LogFormat.colored,
      filter = LogFilter.LogLevelByNameConfig(LogLevel.Trace),
    )))

  final override def run: ZIO[Environment & ZIOAppArgs, Any, Any] =
    runApp.flatMap(exit)
      .tapDefect { cause =>
        ZIO.foreach(cause.defects) { ex =>
          ZIO.succeed { ex.printStackTrace() }
        }
      }
      .tapError { err =>
        err.asInstanceOf[Matchable] match
          case ex: Throwable => ZIO.succeed { ex.printStackTrace() }
          case error => Console.printLineError(error)
        end match
      }

  def runApp: ZIO[Environment & ZIOAppArgs, E, ExitCode]
}
