package dev.argon.platform

import dev.argon.io.*
import zio.ZIO

import java.io.IOException

private[platform] final class PlatformResourceWriter(baseDir: String) extends ResourceWriter {
  override def write[R, E >: IOException](name: String, resource: BinaryResource[R, E]): ZIO[R, E, Unit] = ???
  override def write[R, E >: IOException](name: String, resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, Unit] = ???
}
