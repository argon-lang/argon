package dev.argon.platform


import dev.argon.io.fileio.FileIO
import zio._


object FileIOPlatform {

  val live: ZLayer[Any, Nothing, FileIO] =
    ZLayer.succeed(new NodeIOService())

}
