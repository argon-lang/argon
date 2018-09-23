package com.mi3software.argon.compiler

abstract class ExpressionConverterCombined[TContext <: Context](val context: TContext)
  extends ExpressionConverterContext[TContext]
  with ExpressionConverterConv
{


}
