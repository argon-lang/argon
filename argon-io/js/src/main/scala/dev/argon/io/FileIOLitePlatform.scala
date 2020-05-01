package dev.argon.io

import java.io.IOException

import dev.argon.io.fileio.FileIOLite
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

trait FileIOLitePlatform {

  def nodeLive: ZLayer[Any, Nothing, FileIOLite] =
    ZLayer.succeed(new NodeIOLiteService)

  def browserLive: ZLayer[Any, Nothing, FileIOLite] =
    ZLayer.succeed(new BrowserIOLiteService)

}
