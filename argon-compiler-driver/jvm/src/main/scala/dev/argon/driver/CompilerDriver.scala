package dev.argon.driver

import dev.argon.backend.BackendProvider
import zio.{ExitCode, FiberFailure, LogLevel, Runtime, Unsafe, ZIO}
import dev.argon.backend.api.BackendFactory
import dev.argon.driver.api.CompilerDriverOptions
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

class CompilerDriver extends dev.argon.driver.api.CompilerDriver {
  override def runCommand(options: CompilerDriverOptions): Int =
    val runtime = Runtime.default
    
    Unsafe.unsafely {
      runtime.unsafe.run(
        zio.System.property("dev.argon.backends")
          .flatMap {
            case Some(p) =>
              ZIO.succeed(p.split(java.io.File.pathSeparator).view.map(Path.of(_)).toSeq)

            case None | Some("") =>
              ZIO.fail(new IllegalArgumentException("No backend directories specified"))
          }
          .flatMap { pluginDirs =>
            BackendLoaderUtils.loadAllBackends(pluginDirs)
          }
          .flatMap { backendFactories =>
            CompilerDriverImpl.runCommand(options.getArguments.toSeq)
              .provideLayer(BackendProvider.liveFromFactories(backendFactories))
          }
          .provideLayer(
            (Runtime.removeDefaultLoggers >+> consoleLogger(ConsoleLoggerConfig(
              format = LogFormat.colored,
              filter = LogFilter.LogLevelByNameConfig(LogLevel.Trace),
            ))) ++
              Runtime.enableLoomBasedExecutor ++
              Runtime.enableLoomBasedBlockingExecutor
          )
        
      )
        .getOrElse { cause =>
          throw cause.failureOption
            .map {
              case ex: Throwable => ex
              case _ => FiberFailure(cause)
            }
            .orElse {
              if cause.isInterrupted then
                Some(InterruptedException())
              else
                None
            }
            .orElse {
              cause.defects.headOption
            }
            .getOrElse {
              FiberFailure(cause)
            }
        }
        .code
    }
  end runCommand
}
