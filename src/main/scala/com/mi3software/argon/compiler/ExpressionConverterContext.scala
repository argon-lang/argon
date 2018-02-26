package com.mi3software.argon.compiler

trait ExpressionConverterContext[TContext <: Context] extends ExpressionConverterTypes {
  override val context: TContext
}
