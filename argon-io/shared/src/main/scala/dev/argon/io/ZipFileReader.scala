package dev.argon.io

import dev.argon.stream.builder.Source
import zio._

trait ZipFileReader[-R, +E] {
  def getEntryStream(name: String): Source[R, E, Chunk[Byte], Unit]
}
