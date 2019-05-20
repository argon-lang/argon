package dev.argon.build

import dev.argon.compiler.CompilationError
import dev.argon.util.FileSpec
import dev.argon.util.stream.ArStream

final case class InputFileInfo[F[+_, +_]](fileSpec: FileSpec, dataStream: ArStream[F, scalaz.NonEmptyList[CompilationError], Char])
