package com.mi3software.argon.compiler

final case class Variable[TS <: TypeSystem](descriptor: VariableDescriptor, mutability: Mutability, varType: TS#TType)
