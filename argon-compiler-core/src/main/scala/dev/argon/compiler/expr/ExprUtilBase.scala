package dev.argon.compiler.expr

import dev.argon.compiler.*

trait ExprUtilBase extends UsingContext {
  val exprContext: ArgonExprContext & HasContext[context.type]
}
