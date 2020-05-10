package dev.argon.compiler.expr

import dev.argon.compiler.core.VariableName

import scala.collection.immutable.Vector

final case class ParameterElement[TContext, Wrap[+_]]
(
  paramVar: ParameterVariable[TContext, Wrap],
  name: VariableName,
  elemType: ArExprWrap[TContext, Wrap],
  index: Int
)

final case class Parameter[TContext, Wrap[+_]]
(
  style: ParameterStyle,
  paramVar: ParameterVariable[TContext, Wrap],
  elements: Vector[ParameterElement[TContext, Wrap]]
) {
  def paramType: ArExprWrap[TContext, Wrap] = paramVar.varType
}