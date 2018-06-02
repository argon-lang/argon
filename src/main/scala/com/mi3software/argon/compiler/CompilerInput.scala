package com.mi3software.argon.compiler

import java.io.File

import com.mi3software.argon.parser.SourceAST

final case class CompilerInput
(
  source: Vector[SourceAST],
  references: Vector[File],
)
