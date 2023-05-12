package dev.argon.platform

import dev.argon.compiler.DiagnosticError
import dev.argon.io.JsonResource.DecodeError
import dev.argon.parser.SyntaxError
import dev.argon.plugin.Plugin
import dev.argon.plugins.source.SourcePlugin
import dev.argon.build.BuildError
import dev.argon.plugins.js.{JSPlugin, JSPluginError}
import zio.*
import zio.logging.*

import java.io.IOException
import java.nio.charset.CharacterCodingException

abstract class PlatformApp extends ZIOApp {
  final override type Environment = PathUtil
  type Error = SyntaxError | DiagnosticError | IOException | CharacterCodingException | DecodeError | BuildError | JSPluginError

  final override def environmentTag: EnvironmentTag[Environment] = summon[EnvironmentTag[Environment]]

  final override def bootstrap: ZLayer[ZIOAppArgs, Any, Environment] =
    (Runtime.removeDefaultLoggers >>> consoleLogger(ConsoleLoggerConfig(
      format = LogFormat.colored,
      filter = LogFilter.logLevel(LogLevel.Trace),
    ))) +!+
      ZLayer.succeed(PlatformPathUtil())

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

  def runApp: ZIO[Environment & ZIOAppArgs, Any, ExitCode]

  def plugins[R <: Environment, E >: Error]: Map[String, Plugin[R, E]] =
    Map(
      "source" -> SourcePlugin(),
      "js" -> JSPlugin(),
    )
}
