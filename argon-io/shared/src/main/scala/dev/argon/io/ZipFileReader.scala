package dev.argon.io

import dev.argon.stream.builder.Source
import zio._

trait ZipFileReader[F[_]] {
  def getEntryStream(name: String): Source[F, Chunk[Byte], Unit]
}
