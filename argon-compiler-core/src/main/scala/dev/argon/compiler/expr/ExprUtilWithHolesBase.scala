package dev.argon.compiler.expr

import dev.argon.util.*

trait ExprUtilWithHolesBase extends ExprUtilBase {
  override val exprContext: ArgonExprContext {
    val context: ExprUtilWithHolesBase.this.context.type
    type THole = UniqueIdentifier
  }
}
