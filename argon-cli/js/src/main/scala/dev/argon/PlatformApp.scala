package dev.argon

import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.util.{NodeProcess, NodeSystem}
import zio._
import zio.system.System

trait PlatformApp extends App {

  def run2(args: List[String]): ZIO[ZEnv with FileIO with FileIOLite, Nothing, Int]

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private def getCommandLineArgs: UIO[List[String]] =
    IO.effectTotal { NodeProcess.argv.toArray.drop(2).toList }

  private def baseLayer: ZLayer[Any, Nothing, FileIO with FileIOLite with System] =
    ZLayer.succeed(FileIO.liveNode) ++ FileIOLite.nodeLive ++ NodeSystem.nodeLive


  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    getCommandLineArgs.flatMap(run2).provideSomeLayer[ZEnv](baseLayer)
}
