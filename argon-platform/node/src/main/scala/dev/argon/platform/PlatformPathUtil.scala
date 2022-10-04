package dev.argon.platform

import dev.argon.io.*
import zio.*

import java.io.IOException
import java.nio.file.Path
import typings.node.nodePathMod as NodePath

class PlatformPathUtil extends PathUtil {
  override def dirname(path: String): UIO[String] =
    ZIO.succeed {
      NodePath.dirname(path)
    }

  override def binaryResource(path: String): BinaryResource[Any, IOException] =
    NodeBinaryResource(path)

  override def resourceLayer(path: String): ULayer[ResourceReader & ResourceWriter] =
    ZLayer.succeed(PlatformResourceReader(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
}
