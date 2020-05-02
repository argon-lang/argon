package dev.argon.platform

import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio._
import zio.system.System

trait PlatformApp extends App {

  def runApp(args: List[String]): ZIO[ZEnv with FileIO[FilePath] with FileIOLite, Nothing, Int]

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private def getCommandLineArgs: UIO[List[String]] =
    IO.effectTotal { NodeProcess.argv.toArray.drop(2).toList }

  private def baseLayer: ZLayer[Any, Nothing, FileIO[FilePath] with FileIOLite with System] =
    FileIOPlatform.live ++ FileIOLitePlatform.live ++ NodeSystem.live

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    getCommandLineArgs.flatMap(runApp).provideSomeLayer[ZEnv](baseLayer)

}
