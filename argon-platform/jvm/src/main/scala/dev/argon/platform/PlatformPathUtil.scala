package dev.argon.platform

import dev.argon.io.*
import zio.*

import java.io.IOException
import java.nio.file.Path

class PlatformPathUtil extends PathUtil {
  override def dirname(path: Path): UIO[Path] =
    ZIO.succeed {
      // asInstanceOf needed
      Option(path.toAbsolutePath.nn.getParent)
        .orElse(Option(path.getRoot))
        .get
        .nn
    }

  override def binaryResource(path: Path): BinaryResource[Any, IOException] =
    PathBinaryResource(path)

  override def resourceLayer(path: Path): ULayer[ResourceReader & ResourceWriter] =
    ZLayer.succeed(PlatformResourceReader(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
}
