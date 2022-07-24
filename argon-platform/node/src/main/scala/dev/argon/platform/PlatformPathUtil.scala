package dev.argon.platform

import dev.argon.io.*
import zio.*

import java.nio.file.Path

class PlatformPathUtil extends PathUtil {
  override def fromString(path: String): String =
    path

  override def dirname(path: String): UIO[String] =
    ZIO.succeed {
      NodePath.dirname(path)
    }

  override def resourceLayer(path: String): ULayer[ResourceFactory & ResourceWriter] =
    ZLayer.succeed(PlatformResourceFactory(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
}
