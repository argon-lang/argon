package dev.argon.platform

import dev.argon.io.fileio.FileIOLite
import zio._

private[platform] object FileIOLitePlatform {

  def live: ZLayer[Any, Nothing, FileIOLite] =
    ZLayer.succeed(new NodeIOLiteService)

}
