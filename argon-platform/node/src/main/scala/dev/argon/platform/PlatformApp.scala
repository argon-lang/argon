package dev.argon.platform

import dev.argon.io.fileio.{FileIO, ZipRead}
import zio._
import zio.system.System

trait PlatformApp extends App {

  def runApp(args: List[String]): ZIO[ZEnv with FileIO with ZipRead, Nothing, ExitCode]

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private def getCommandLineArgs: UIO[List[String]] =
    IO.effectTotal { NodeProcess.argv.toArray.drop(2).toList }

  private def baseLayer: ZLayer[Any, Nothing, FileIO with ZipRead with System] =
    FileIOPlatform.live ++ ZipReadPlatform.live ++ NodeSystem.live

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    getCommandLineArgs.flatMap(runApp).provideSomeLayer[ZEnv](baseLayer)

}
