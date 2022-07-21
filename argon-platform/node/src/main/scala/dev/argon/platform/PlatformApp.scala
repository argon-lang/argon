package dev.argon.platform

import zio.*

abstract class PlatformApp extends ZIOApp {
  final override type Environment = Any

  final override def environmentTag: EnvironmentTag[Environment] = summon[EnvironmentTag[Environment]]

  final override def bootstrap: ZLayer[ZIOAppArgs with Scope, Any, Environment] =
    ZLayer.succeed(())

  final override def run: ZIO[Environment & ZIOAppArgs, Any, Any] =
    runApp
      .provideSomeLayer[Environment & ZIOAppArgs](NodeSystemLayer.live)
      .provideSomeLayer[Environment & ZIOAppArgs](ZLayer.succeed(ZIOAppArgs(Chunk.fromIterable(NodeProcess.argv))))
      .onExit {
        case Exit.Success(exitCode) => ZIO.succeed { NodeProcess.exitCode = exitCode.code }
        case Exit.Failure(_) => ZIO.succeed { NodeProcess.exitCode = 1 }
      }

  def runApp: ZIO[Environment & ZIOAppArgs, Any, ExitCode]
}
