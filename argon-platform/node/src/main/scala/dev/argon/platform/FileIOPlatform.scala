package dev.argon.platform

import java.io.IOException

import dev.argon.io.fileio.FileIO
import zio.stream.{Stream, ZStream}
import zio._
import cats.implicits._


object FileIOPlatform {

  val live: ZLayer[Any, Nothing, FileIO] =
    ZLayer.succeed(new NodeIOService())

}
