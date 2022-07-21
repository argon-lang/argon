package dev.argon.platform

import zio.*

abstract class PlatformApp extends ZIOApp {
  final override type Environment = Any

  final override def environmentTag: EnvironmentTag[Environment] = summon[EnvironmentTag[Environment]]

  final override def bootstrap: ZLayer[ZIOAppArgs with Scope, Any, Environment] =
    ZLayer.succeed(())

  final override def run: ZIO[Environment & ZIOAppArgs, Any, Any] =
    runApp.flatMap(exit)

  def runApp: ZIO[Environment & ZIOAppArgs, Any, ExitCode]
}
