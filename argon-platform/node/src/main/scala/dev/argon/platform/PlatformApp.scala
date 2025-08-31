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
      .provide[Environment](ZLayer.succeed(ZIOAppArgs(Chunk.fromIterable(NodeProcess.argv.jsSlice(2)))))
      .foldCauseZIO(
        failure = cause => {
          ZIO.succeed {
            println(cause);
            cause.defects.foreach(ex => ex.printStackTrace())
            NodeProcess.exitCode = 1
          }
        },
        success = exitCode => {
          ZIO.succeed { NodeProcess.exitCode = exitCode.code }
        },
      )
//      .provideSomeLayer(
//        Runtime.removeDefaultLoggers >>> Runtime.addLogger(ConsoleL)
//      )

  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode]
}
