package dev.argon.io

import zio.*
import zio.stream.*
import java.io.IOException

sealed trait Resource[+E]

trait DirectoryResource[+E] extends Resource[E] {
  def contents: Stream[E, DirectoryEntry[E]]
}

final case class DirectoryEntry[+E](name: String, resource: Resource[E])

trait BinaryResource[+E] extends Resource[E] with BinaryResourcePlatformSpecific[E] {
  def asBytes: Stream[E, Byte]
}

trait TextResource[+E] extends BinaryResource[E] {
  def asText: Stream[E, String]

  override def asBytes: Stream[E, Byte] = ZPipeline.utf8Encode.orDie.apply(asText)
}
