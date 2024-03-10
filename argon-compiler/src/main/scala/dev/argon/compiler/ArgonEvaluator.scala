package dev.argon.compiler

import dev.argon.expr.Evaluator
import dev.argon.util.Fuel
import zio.ZIO

object ArgonEvaluator {
  def apply(ctx: Context): Evaluator[ctx.Env, ctx.Error] { val exprContext: ctx.DefaultExprContext.type } =
    new Evaluator[ctx.Env, ctx.Error] with UsingContext {
      override val context: ctx.type = ctx

      import context.DefaultExprContext.*
      override val exprContext: context.DefaultExprContext.type = context.DefaultExprContext

      override def getFunctionBody(function: ArFunc, args: Seq[Expr], fuel: Fuel): Comp[(Expr, Boolean)] =
        ZIO.succeed((Expr.FunctionCall(function, args), false))

      override def normalizeHole(hole: Hole): Comp[Expr] = hole

    }
}
