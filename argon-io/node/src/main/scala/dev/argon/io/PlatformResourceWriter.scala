package dev.argon.io

import dev.argon.io.*
import zio.ZIO

import java.io.IOException

private[io] final class PlatformResourceWriter(baseDir: String) extends ResourceWriter {
  override def write[E >: IOException](name: String, resource: BinaryResource[E]): ZIO[Any, E, Unit] = ???
  override def write[E >: IOException](name: String, resource: DirectoryResource[E, BinaryResource]): ZIO[Any, E, Unit] = ???
}
