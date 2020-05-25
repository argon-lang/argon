package dev.argon.compiler.expr

import cats.Eq
import cats.implicits._
import dev.argon.compiler.core._

sealed trait Variable[TContext, Wrap[+_]] {
  val owner: VariableOwner[TContext]
  val name: VariableName
  val mutability: Mutability
  val varType: ArExprWrap[TContext, Wrap]
}

object Variable {
  implicit def eqInstance[TContext <: Context with Singleton, Wrap[+_]]: Eq[Variable[TContext, Wrap]] = new Eq[Variable[TContext, Wrap]] {
    override def eqv(x: Variable[TContext, Wrap], y: Variable[TContext, Wrap]): Boolean =
      (x, y) match {
        case (x: LocalVariable[TContext, Wrap], y: LocalVariable[TContext, Wrap]) => x.id === y.id
        case (_: LocalVariable[TContext, Wrap], _) | (_, _: LocalVariable[TContext, Wrap]) => false


        case (x: ParameterVariable[TContext, Wrap], y: ParameterVariable[TContext, Wrap]) => x.owner === y.owner
        case (_: ParameterVariable[TContext, Wrap], _) | (_, _: ParameterVariable[TContext, Wrap]) => false

        case (x: FieldVariable[TContext, Wrap], y: FieldVariable[TContext, Wrap]) => x.owner.ownerClass.value.id === y.owner.ownerClass.value.id
      }
  }
}

final case class LocalVariable[TContext, Wrap[+_]]
(
  id: VariableId,
  owner: LocalVariableOwner[TContext],
  name: VariableName,
  mutability: Mutability,
  varType: ArExprWrap[TContext, Wrap]
) extends Variable[TContext, Wrap]

final case class ParameterVariable[TContext, Wrap[+_]]
(
  owner: ParameterVariableOwner[TContext],
  index: Int,
  name: VariableName,
  mutability: Mutability,
  varType: ArExprWrap[TContext, Wrap]
) extends Variable[TContext, Wrap]

final case class FieldVariable[TContext <: Context with Singleton, Wrap[+_]]
(
  owner: FieldVariableOwner[TContext],
  name: VariableName.Normal,
  mutability: Mutability,
  varType: ArExprWrap[TContext, Wrap]
) extends Variable[TContext, Wrap]
