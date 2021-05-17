package dev.argon.platform

import zio._

trait PlatformApp extends App {

  def runApp(args: List[String]): ZIO[ZEnv, Nothing, ExitCode]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    runApp(args)

}
