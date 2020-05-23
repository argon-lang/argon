package dev.argon.io

import zio.stream.ZStream

final case class ZipEntryInfo[-R, +E](path: String, dataStream: ZStream[R, E, Byte])
