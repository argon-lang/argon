package dev.argon.compiler

import dev.argon.parser.SourceAST

import cats._

final case class CompilerInput[I, B]
(
  source: Vector[SourceAST],
  references: Vector[I],
  options: CompilerOptions[Id],
  backendOptions: B,
)
