package com.mi3software.argon.compiler

trait ConvScopeTypes[TContext <: Context, TS <: TypeSystem] extends ScopeTypes {
  override type TTrait = ArTrait[TContext]
  override type TClass = ArClass[TContext]
  override type TDataConstructor = DataConstructor[TContext]
  override type TFunc = ArFunc[TContext]
  override type TVariable = Variable[TS, VariableLikeDescriptor]
}
