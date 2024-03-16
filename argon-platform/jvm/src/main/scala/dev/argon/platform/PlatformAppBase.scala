package dev.argon.platform

import zio.*
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}

trait PlatformAppBase[R, E, AppLayerIn >: ZIOAppArgs] extends ZIOApp {
  override type Environment = R
  type Error = E

  def appBootstrapLayer: ZLayer[AppLayerIn, E, R]

  final override def bootstrap: ZLayer[AppLayerIn, Any, R] =
    (Runtime.removeDefaultLoggers >+> consoleLogger(ConsoleLoggerConfig(
      format = LogFormat.colored,
      filter = LogFilter.LogLevelByNameConfig(LogLevel.Trace),
    ))) >>> appBootstrapLayer
}
