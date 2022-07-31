package dev.argon.compiler.definitions

import dev.argon.compiler.*

sealed trait FunctionImplementationC extends UsingContext
object FunctionImplementationC {
  trait External extends FunctionImplementationC {
    val impl: context.ExternFunctionImplementation
  }
  trait ExpressionBody extends FunctionImplementationC {
    val body: context.ExprContext.WrapExpr
  }
}
