package dev.argon.platform

import dev.argon.io.*
import zio.stream.ZStream

import java.io.IOException
import java.nio.file.{Files, Path}

private[platform] final class PlatformResourceReader(baseDir: Path) extends ResourceReader {
  override def directoryResource(name: String): DirectoryResource[Any, IOException, BinaryResource] =
    PathDirectoryResource(baseDir.resolve(name).nn)


  override def binaryResource(name: String): BinaryResource[Any, IOException] =
    PathBinaryResource(baseDir.resolve(name).nn)
}
