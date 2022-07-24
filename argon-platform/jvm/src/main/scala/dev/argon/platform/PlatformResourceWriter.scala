package dev.argon.platform

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.Path

private[platform] final class PlatformResourceWriter(baseDir: Path) extends ResourceWriter {
  override def write[R, E >: IOException](name: String, resource: BinaryResource[R, E]): ZIO[R, E, Unit] = ???
  override def write[R, E >: IOException](name: String, resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, Unit] = ???
}
