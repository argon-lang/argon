package dev.argon.platform

import java.io.IOException

import dev.argon.io.fileio.FileIOLite
import zio._

private[platform] object FileIOLitePlatform {

  def live: ZLayer[Any, Nothing, FileIOLite] =
    ZLayer.succeed(new BrowserIOLiteService)

}
