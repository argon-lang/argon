package dev.argon

import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio._
import zio.blocking.Blocking

trait PlatformApp extends App {
  def run2(args: List[String]): ZIO[ZEnv with FileIO with FileIOLite, Nothing, Int]

  private def baseLayer: ZLayer[Blocking, Nothing, FileIO with FileIOLite] = FileIO.live ++ FileIOLite.live


  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    run2(args).provideSomeLayer[ZEnv](baseLayer)

}
