package dev.argon.compiler

import dev.argon.parser.SourceAST

import cats._
import zio.stream._

final case class CompilerInput[I, B]
(
  source: Stream[ErrorList, SourceAST],
  references: Vector[I],
  options: CompilerOptions[Id],
  backendOptions: B,
)
