package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.types._

sealed trait VariableName
object VariableName {
  final case class Normal(name: String) extends VariableName
  case object Unnamed extends VariableName
}

trait VariableContext {

  val typeSystem: TypeSystem


  final case class Variable[+Desc <: VariableLikeDescriptor](descriptor: Desc, name: VariableName, mutability: Mutability, varType: typeSystem.TType)

  final case class Parameter(tupleVars: Vector[Variable[DeconstructedParameterDescriptor]])

  object Parameter {

    def paramType(param: Parameter): typeSystem.TType =
      typeSystem.fromSimpleType(typeSystem.TupleType(param.tupleVars.map { tupleVar => typeSystem.TupleTypeElement(tupleVar.varType) }))

  }

}

