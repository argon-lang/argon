package dev.argon.platform

import dev.argon.io.*

import java.io.IOException

private[platform] final class PlatformResourceReader(baseDir: String) extends ResourceReader {
  override def directoryResource(name: String): DirectoryResource[Any, IOException, BinaryResource] =
    NodeDirectoryResource(name)

  override def binaryResource(name: String): BinaryResource[Any, IOException] =
    NodeBinaryResource(name)
}
