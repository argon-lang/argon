package dev.argon.io

import java.io.IOException
import zio.*

trait ResourceWriter {
  def write[R, E >: IOException](name: String, resource: BinaryResource[R, E]): ZIO[R, E, Unit]
  def write[R, E >: IOException](name: String, resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, Unit]
}
