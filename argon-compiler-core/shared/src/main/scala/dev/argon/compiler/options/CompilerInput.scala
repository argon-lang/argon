package dev.argon.compiler.options

import cats._
import dev.argon.options.{OptionID, Options}

final case class CompilerInput[B <: OptionID]
(
  options: Options[Id, CompilerOptionID],
  backendOptions: Options[Id, B],
)
