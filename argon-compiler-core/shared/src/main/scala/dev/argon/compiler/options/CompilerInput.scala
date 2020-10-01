package dev.argon.compiler.options

import cats._
import dev.argon.compiler.CompilationError
import dev.argon.parser.SourceAST
import zio.stream._

final case class CompilerInput[I, B]
(
  options: CompilerOptions[Id, I],
  backendOptions: B,
)
