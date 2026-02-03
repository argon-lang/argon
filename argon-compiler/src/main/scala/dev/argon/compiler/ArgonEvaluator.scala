package dev.argon.compiler

import dev.argon.expr.Evaluator

object ArgonEvaluator {
  def apply(ctx: Context): Evaluator[ctx.Env, ctx.Error] { val exprContext: ctx.DefaultExprContext.type } =
    new ArgonEvaluatorBase[ctx.Env, ctx.Error] {
      override val context: ctx.type = ctx

      import context.DefaultExprContext.*
      override val exprContext: context.DefaultExprContext.type = context.DefaultExprContext
      override protected val signatureContext: context.DefaultSignatureContext.type = context.DefaultSignatureContext

      override protected def shiftExpr(expr: context.DefaultExprContext.Expr): exprContext.Expr =
        expr

      override protected def shiftSig(sig: context.DefaultSignatureContext.FunctionSignature): signatureContext.FunctionSignature =
        sig

      override def normalizeHole(hole: Hole): Comp[Expr] = hole
    }
}
