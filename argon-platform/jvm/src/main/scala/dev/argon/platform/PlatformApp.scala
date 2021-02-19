package dev.argon.platform

import dev.argon.io.fileio.{FileIO, ZipRead}
import zio._

trait PlatformApp extends App {

  def runApp(args: List[String]): ZIO[ZEnv with FileIO with ZipRead, Nothing, ExitCode]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    runApp(args).provideCustomLayer(FileIOPlatform.live ++ ZipReadPlatform.live)

}
