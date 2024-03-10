package dev.argon.io

import java.io.IOException
import zio.*

trait ResourceWriter {
  def write[E >: IOException](name: String, resource: BinaryResource[E]): ZIO[Any, E, Unit]
  def write[E >: IOException](name: String, resource: DirectoryResource[E, BinaryResource]): ZIO[Any, E, Unit]
}
