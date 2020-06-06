package dev.argon.platform

import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio._
import zio.system.System

trait PlatformApp extends App {

  def runApp(args: List[String]): ZIO[ZEnv with FileIOLite, Nothing, ExitCode]

  private def baseLayer: ZLayer[Any, Nothing, FileIOLite] =
    FileIOLitePlatform.live

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    runApp(args).provideSomeLayer[ZEnv](baseLayer)

}
