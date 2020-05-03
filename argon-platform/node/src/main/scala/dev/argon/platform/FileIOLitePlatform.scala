package dev.argon.platform

import java.io.IOException

import dev.argon.io.fileio.FileIOLite
import dev.argon.stream.builder.Source
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._

private[platform] object FileIOLitePlatform {

  def live: ZLayer[Any, Nothing, FileIOLite] =
    ZLayer.succeed(new NodeIOLiteService)

}
