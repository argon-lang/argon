package com.mi3software.argon.compiler

sealed trait VariableName
object VariableName {
  final case class Normal(name: String) extends VariableName
  case object Unnamed extends VariableName
}

final case class Variable[TS <: TypeSystem, +Desc <: VariableLikeDescriptor](descriptor: Desc, name: VariableName, mutability: Mutability, varType: TS#TType)

