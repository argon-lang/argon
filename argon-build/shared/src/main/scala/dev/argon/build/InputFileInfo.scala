package dev.argon.build

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError
import dev.argon.stream.builder.Source
import dev.argon.util.FileSpec

final case class InputFileInfo[F[_]](fileSpec: FileSpec, dataStream: Source[F, Char, Unit])
