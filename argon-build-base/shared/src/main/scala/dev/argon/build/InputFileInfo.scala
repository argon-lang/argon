package dev.argon.build

import dev.argon.util.FileSpec
import zio.stream.ZStream

final case class InputFileInfo[-R, +E](fileSpec: FileSpec, dataStream: ZStream[R, E, Char])
