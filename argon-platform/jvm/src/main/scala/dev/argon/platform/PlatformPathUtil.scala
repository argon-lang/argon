package dev.argon.platform

import dev.argon.io.*
import zio.*

import java.nio.file.Path

class PlatformPathUtil extends PathUtil {
  override def fromString(path: String): Path =
    Path.of(path)

  override def dirname(path: Path): UIO[Path] =
    ZIO.succeed {
      Option(path.toAbsolutePath.getParent).getOrElse(path.getRoot)
    }

  override def resourceLayer(path: Path): ULayer[ResourceFactory & ResourceWriter] =
    ZLayer.succeed(PlatformResourceFactory(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
}
