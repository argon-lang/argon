package dev.argon

import dev.argon.io.fileio.FileIO
import zio.ZLayer
import zio.blocking.Blocking

object PlatformHelpers {
  def fileIOLayer: ZLayer[Blocking, Nothing, FileIO] = FileIO.live

  def getCommandLineArgs(args: List[String]): List[String] =
    args
}
