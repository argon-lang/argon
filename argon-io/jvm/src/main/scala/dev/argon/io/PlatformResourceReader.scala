package dev.argon.io

import dev.argon.io.*
import zio.stream.ZStream

import java.io.IOException
import java.nio.file.{Files, Path}

private[io] final class PlatformResourceReader(baseDir: Path) extends ResourceReader {
  override def directoryResource(name: String): DirectoryResource[IOException, BinaryResource] =
    PathDirectoryResource(baseDir.resolve(name).nn)


  override def binaryResource(name: String): BinaryResource[IOException] =
    PathBinaryResource(baseDir.resolve(name).nn)
}
