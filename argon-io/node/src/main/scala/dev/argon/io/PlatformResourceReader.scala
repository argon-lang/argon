package dev.argon.io

import dev.argon.io.*

import java.io.IOException

private[io] final class PlatformResourceReader(baseDir: String) extends ResourceReader {
  override def directoryResource(name: String): DirectoryResource[IOException, BinaryResource] =
    NodeDirectoryResource(name)

  override def binaryResource(name: String): BinaryResource[IOException] =
    NodeBinaryResource(name)
}
