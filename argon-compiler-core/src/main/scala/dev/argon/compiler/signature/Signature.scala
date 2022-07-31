package dev.argon.compiler.signature

import dev.argon.compiler.expr.CompleteExprContext
import dev.argon.parser.{FunctionParameterListType, IdentifierExpr}

enum Signature[Type, Res] {
  case Parameter(paramListType: FunctionParameterListType, isErased: Boolean, name: Option[IdentifierExpr], paramType: Type, next: Signature[Type, Res])
  case Result(resultType: Res)

  def parameterCount: Int =
    this match {
      case Parameter(_, _, _, _, next) => next.parameterCount + 1
      case Result(_) => 0
    }

  def parameterTypes: Seq[Type] =
    this match {
      case Parameter(_, _, _, p, next) => p +: next.parameterTypes
      case Result(_) => Seq.empty
    }

  def unsubstitutedResult: Res =
    this match {
      case Parameter(_, _, _, _, next) => next.unsubstitutedResult
      case Result(res) => res
    }

}
