package dev.argon.compiler.definitions

import dev.argon.compiler.*

sealed trait MethodImplementationC extends UsingContext
object MethodImplementationC {
  trait Abstract extends MethodImplementationC
  trait External extends MethodImplementationC {
    val impl: context.ExternMethodImplementation
  }
  trait ExpressionBody extends MethodImplementationC {
    val body: context.ExprContext.WrapExpr
  }
}
