package com.mi3software.argon.compiler

object ArgonToHoleScopeTypeConverter {
  def apply[TS <: TypeSystem](context: Context): ScopeTypeConverter[context.ContextScopeTypes, ConvScopeTypes[context.type, TS]] =
    new ScopeTypeConverter[context.ContextScopeTypes, ConvScopeTypes[context.type, TS]] {
      override def convertTrait(t: context.ContextScopeTypes#TTrait): ArTrait[context.type] = t

      override def convertClass(t: context.ContextScopeTypes#TClass): ArClass[context.type] = t

      override def convertDataConstructor(t: context.ContextScopeTypes#TDataConstructor): DataConstructor[context.type] = t

      override def convertFunc(t: context.ContextScopeTypes#TFunc): ArFunc[context.type] = t

      override def convertVariable(t: Nothing): Variable[TS, VariableLikeDescriptor] = t
    }
}
