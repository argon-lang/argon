package com.mi3software.argon.compiler

import com.mi3software.argon.parser.SourceAST

import scalaz.Scalaz.Id

final case class CompilerInput[I, B]
(
  source: Vector[SourceAST],
  references: Vector[I],
  options: CompilerOptions[Id],
  backendOptions: B,
)
