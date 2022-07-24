package dev.argon.io

import zio.*

trait ResourceWriter[+E0] {
  def write[R, E >: E0](name: String, resource: BinaryResource[R, E]): ZIO[R, E, Unit]
  def write[R, E >: E0](name: String, resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, Unit]
}
