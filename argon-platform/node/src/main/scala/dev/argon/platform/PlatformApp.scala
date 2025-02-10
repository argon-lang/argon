package dev.argon.platform

import zio.*

abstract class PlatformApp[E] extends ZIOApp {
  override type Environment = Any
  type Error = E
  override def environmentTag: EnvironmentTag[Any] = summon[EnvironmentTag[Any]]

  final override def bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    ZLayer.empty

  final override def run: ZIO[Environment & ZIOAppArgs, Any, Any] =
    runApp
      .provideSome[Environment](ZLayer.succeed(ZIOAppArgs(Chunk.fromIterable(NodeProcess.argv.jsSlice(2)))))
      .onExit {
        case Exit.Success(exitCode) => ZIO.succeed { NodeProcess.exitCode = exitCode.code }
        case Exit.Failure(_) => ZIO.succeed { NodeProcess.exitCode = 1 }
      }

  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode]
}
