package com.mi3software.argon.compiler

final case class Parameter[+TS <: TypeSystem](tupleVars: Vector[Variable[TS, DeconstructedParameterDescriptor]])

object Parameter {

  def paramType(typeSystem: TypeSystem)(param: Parameter[typeSystem.type]): typeSystem.TType =
    typeSystem.fromTypeBase(TupleType(param.tupleVars.map { tupleVar => TupleTypeElement[typeSystem.type](tupleVar.varType) }))

}
