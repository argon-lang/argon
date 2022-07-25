package dev.argon.platform

import dev.argon.io.*
import zio.*

import java.io.IOException
import java.nio.file.Path

class PlatformPathUtil extends PathUtil {
  override def dirname(path: String): UIO[String] =
    ZIO.succeed {
      NodePath.dirname(path)
    }

  override def binaryResource(path: String): BinaryResource[Any, IOException] =
    NodeBinaryResource(path)

  override def resourceLayer(path: String): ULayer[ResourceFactory & ResourceWriter] =
    ZLayer.succeed(PlatformResourceFactory(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
}
