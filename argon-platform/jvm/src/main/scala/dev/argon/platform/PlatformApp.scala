package dev.argon.platform

import zio.*

abstract class PlatformApp[R: EnvironmentTag, E] extends PlatformAppBase[R, E, ZIOAppArgs] {
  override def environmentTag: EnvironmentTag[R] = summon[EnvironmentTag[R]]

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
