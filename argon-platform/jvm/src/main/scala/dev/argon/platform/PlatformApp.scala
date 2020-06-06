package dev.argon.platform

import dev.argon.io.Path
import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio._

trait PlatformApp extends App {

  def runApp(args: List[String]): ZIO[ZEnv with FileIO[FilePath] with FileIOLite, Nothing, ExitCode]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    runApp(args).provideCustomLayer(FileIOPlatform.live ++ FileIOLitePlatform.live)

}
