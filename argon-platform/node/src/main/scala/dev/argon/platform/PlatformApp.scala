package dev.argon.platform

import zio.*

abstract class PlatformApp[R: EnvironmentTag, E] extends ZIOApp {
  override type Environment = R
  override def environmentTag: EnvironmentTag[R] = summon[EnvironmentTag[R]]

  type Error = E

  def appBootstrapLayer: ZLayer[ZIOAppArgs, E, R]

  final override def bootstrap: ZLayer[ZIOAppArgs, Any, R] =
    ZLayer.succeed(ZIOAppArgs(Chunk.fromIterable(NodeProcess.argv))) >>>
      appBootstrapLayer +!+ ZLayer.environment[ZIOAppArgs]

  final override def run: ZIO[Environment & ZIOAppArgs, Any, Any] =
    runApp.onExit {
      case Exit.Success(exitCode) => ZIO.succeed { NodeProcess.exitCode = exitCode.code }
      case Exit.Failure(_) => ZIO.succeed { NodeProcess.exitCode = 1 }
    }

  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode]
}
