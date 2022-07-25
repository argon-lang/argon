package dev.argon.platform

import dev.argon.io.*
import zio.*

import java.io.IOException
import java.nio.file.Path

class PlatformPathUtil extends PathUtil {
  override def dirname(path: Path): UIO[Path] =
    ZIO.succeed {
      Option(path.toAbsolutePath.getParent).getOrElse(path.getRoot)
    }

  override def binaryResource(path: Path): BinaryResource[Any, IOException] =
    PathBinaryResource(path)

  override def resourceLayer(path: Path): ULayer[ResourceFactory & ResourceWriter] =
    ZLayer.succeed(PlatformResourceFactory(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
}
