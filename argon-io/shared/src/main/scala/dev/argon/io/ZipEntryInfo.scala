package dev.argon.io

import dev.argon.stream.ArStream

final case class ZipEntryInfo[F[-_, +_, +_], -R, +E](path: String, dataStream: ArStream[F, R, E, Byte])
