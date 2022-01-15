package dev.argon.compiler.signature

import dev.argon.parser.FunctionParameterListType

enum Signature[Type, Res] {
  case Parameter(paramListType: FunctionParameterListType, paramType: Type, next: Signature[Type, Res])
  case Result(resultType: Res)

  def parameterCount: Int =
    this match {
      case Parameter(_, _, next) => next.parameterCount + 1
      case Result(_) => 0
    }

  def parameterTypes: Seq[Type] =
    this match {
      case Parameter(_, p, next) => p +: next.parameterTypes
      case Result(_) => Seq.empty
    }
}
