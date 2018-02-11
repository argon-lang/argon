package com.mi3software.argon.compiler

final case class Variable[TS <: TypeSystem, +Desc <: VariableLikeDescriptor](descriptor: Desc, mutability: Mutability, varType: TS#TType)

