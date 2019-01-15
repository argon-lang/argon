package com.mi3software.argon.build

import com.mi3software.argon.util.FileSpec
import com.mi3software.argon.util.stream.ArStream

final case class InputFileInfo[F[_]](fileSpec: FileSpec, dataStream: ArStream[F, Char, Unit])
