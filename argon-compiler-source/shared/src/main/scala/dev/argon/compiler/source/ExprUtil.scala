package dev.argon.compiler.source

import dev.argon.compiler.expr.ArgonExprContext

object ExprUtil {
  def referencesVariable(exprContext: ArgonExprContext)(variable: exprContext.Variable)(expr: exprContext.WrapExpr): Boolean =
    import exprContext._
    expr match {
      case WrapExpr.OfExpr(e) =>
        val hasVariableTop = e.constructor match {
          case ExprConstructor.LoadVariable(v) => variable == v
          case ExprConstructor.StoreVariable(v) => variable == v
          case _ => false
        }

        if hasVariableTop then
          true
        else
          e.constructor.argsToExprs(e.args).exists(referencesVariable(exprContext)(variable))

      case WrapExpr.OfHole(_) => false
    }
  end referencesVariable
}
