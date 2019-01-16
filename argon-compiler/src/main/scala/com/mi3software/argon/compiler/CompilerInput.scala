package com.mi3software.argon.compiler

import com.mi3software.argon.parser.SourceAST

final case class CompilerInput[I]
(
  source: Vector[SourceAST],
  references: Vector[I],
  options: CompilerOptions,
)
