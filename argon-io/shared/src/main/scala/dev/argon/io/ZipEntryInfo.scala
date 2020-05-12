package dev.argon.io

import dev.argon.stream.builder.Source
import zio.Chunk

final case class ZipEntryInfo[-R, +E](path: String, dataStream: Source[R, E, Chunk[Byte], Unit])
