package dev.argon

import dev.argon.io.fileio.FileIO
import zio.ZLayer

object PlatformHelpers {
  def fileIOLayer: ZLayer[Any, Nothing, FileIO] = ZLayer.succeed(FileIO.liveNode)

  def getCommandLineArgs(args: List[String]): List[String] =
    NodeProcess.argv.toArray.drop(2).toList
}
