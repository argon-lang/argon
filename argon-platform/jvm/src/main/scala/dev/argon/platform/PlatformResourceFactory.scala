package dev.argon.platform

import dev.argon.io.*
import zio.stream.ZStream

import java.io.IOException
import java.nio.file.{Files, Path}

private[platform] final class PlatformResourceFactory(baseDir: Path) extends ResourceFactory {
  override def directoryResource(name: String): DirectoryResource[Any, IOException, BinaryResource] =
    PathDirectoryResource(baseDir.resolve(name))


  override def binaryResource(name: String): BinaryResource[Any, IOException] = ???
}
