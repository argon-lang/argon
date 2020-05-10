package dev.argon.compiler.expr

import dev.argon.compiler.core._

sealed trait Variable[TContext, Wrap[+_]] {
  val descriptor: VariableLikeDescriptor
  val name: VariableName
  val mutability: Mutability
  val varType: ArExprWrap[TContext, Wrap]
}

final case class LocalVariable[TContext, Wrap[+_]]
(
  descriptor: VariableDescriptor,
  name: VariableName,
  mutability: Mutability,
  varType: ArExprWrap[TContext, Wrap]
) extends Variable[TContext, Wrap]

final case class ParameterVariable[TContext, Wrap[+_]]
(
  descriptor: ParameterDescriptor,
  name: VariableName,
  mutability: Mutability,
  varType: ArExprWrap[TContext, Wrap]
) extends Variable[TContext, Wrap]

final case class FieldVariable[TContext <: Context with Singleton, Wrap[+_]]
(
  descriptor: FieldDescriptor,
  ownerClass: AbsRef[TContext, ArClass],
  name: VariableName.Normal,
  mutability: Mutability,
  varType: ArExprWrap[TContext, Wrap]
) extends Variable[TContext, Wrap]
