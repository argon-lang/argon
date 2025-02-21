package dev.argon.backend.options

import dev.argon.io.{BinaryResource, FileSystemResource}
import zio.*

trait OutputProvider[E, Output] {
  def outputs(output: Output): IO[E, Map[String, FileSystemResource[E, BinaryResource]]]
}
