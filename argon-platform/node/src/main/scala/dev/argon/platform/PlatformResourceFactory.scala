package dev.argon.platform

import dev.argon.io.*

import java.io.IOException

private[platform] final class PlatformResourceFactory(baseDir: String) extends ResourceFactory {
  override def directoryResource(name: String): DirectoryResource[Any, IOException, BinaryResource] =
    NodeDirectoryResource(name)

  override def binaryResource(name: String): BinaryResource[Any, IOException] =
    NodeBinaryResource(name)
}
