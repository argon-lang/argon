package dev.argon.build

import dev.argon.util.FileSpec
import dev.argon.util.stream.ArStream

final case class InputFileInfo[F[_]](fileSpec: FileSpec, dataStream: ArStream[F, Char, Unit])
