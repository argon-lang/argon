package dev.argon.build

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError
import dev.argon.stream.ArStream
import dev.argon.util.FileSpec

final case class InputFileInfo[L[_, _]](fileSpec: FileSpec, dataStream: L[Char, Unit])
