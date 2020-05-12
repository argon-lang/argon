package dev.argon.io

import dev.argon.stream.builder.Source
import zio.Chunk
import zio.stream.ZStream

final case class ZipEntryInfo[-R, +E](path: String, dataStream: ZStream[R, E, Chunk[Byte]])
