package com.mi3software.argon.compiler

trait ExpressionConverterContext[TContext <: Context] extends ExpressionConverter {
  override val context: TContext
}
