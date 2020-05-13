package dev.argon.build

import cats.data.NonEmptyList
import dev.argon.compiler.CompilationError
import dev.argon.stream.builder.Source
import dev.argon.util.FileSpec
import zio.stream.ZStream

final case class InputFileInfo[-R, +E](fileSpec: FileSpec, dataStream: ZStream[R, E, Char])
