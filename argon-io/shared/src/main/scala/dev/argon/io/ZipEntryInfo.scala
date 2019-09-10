package dev.argon.io

import dev.argon.stream.builder.Source
import zio.Chunk

final case class ZipEntryInfo[F[_]](path: String, dataStream: Source[F, Chunk[Byte], Unit])
