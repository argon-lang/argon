package dev.argon.compiler.source

import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.compiler.signature.Signature

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


  def substituteSignature[Res]
  (exprContext: ArgonExprContext)
  (variable: exprContext.Variable)
  (replacement: exprContext.WrapExpr)
  (substResult: Res => Res)
  (sig: Signature[exprContext.WrapExpr, Res])
  : Signature[exprContext.WrapExpr, Res] =
    sig match {
      case Signature.Parameter(listType, paramType, next) =>
        Signature.Parameter(
          listType,
          substituteWrapExpr(exprContext)(variable)(replacement)(paramType),
          substituteSignature(exprContext)(variable)(replacement)(substResult)(next)
        )

      case Signature.Result(res) =>
        Signature.Result(substResult(res))
    }

  def substituteWrapExpr
  (exprContext: ArgonExprContext)
  (variable: exprContext.Variable)
  (replacement: exprContext.WrapExpr)
  (expr: exprContext.WrapExpr)
  : exprContext.WrapExpr = ???
}
