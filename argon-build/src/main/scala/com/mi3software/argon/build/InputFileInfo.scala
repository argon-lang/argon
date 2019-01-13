package com.mi3software.argon.build

import com.mi3software.argon.util.FileSpec
import fs2.Stream

final case class InputFileInfo[F[_]](fileSpec: FileSpec, dataStream: Stream[F, Char])
