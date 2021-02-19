package dev.argon.compiler.options

import cats._
import dev.argon.compiler.CompilationError
import dev.argon.options.{OptionID, Options}
import dev.argon.parser.SourceAST
import zio.stream._

final case class CompilerInput[B <: OptionID]
(
  options: Options[Id, CompilerOptionID],
  backendOptions: Options[Id, B],
)
