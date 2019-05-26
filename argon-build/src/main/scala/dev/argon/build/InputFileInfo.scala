package dev.argon.build

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError
import dev.argon.util.FileSpec
import dev.argon.util.stream.ArStream

final case class InputFileInfo[F[-_, +_, +_], R](fileSpec: FileSpec, dataStream: ArStream[F, R, NonEmptyList[CompilationError], Char])
