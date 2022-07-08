package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.expr.Evaluator
import zio.*

abstract class ArgonEvaluator extends UsingContext with Evaluator[CompEnv, CompError] {
  override val exprContext: ArgonExprContext with HasContext[context.type]
  import exprContext.WrapExpr

  override def getFunctionBody(function: ArFunc, args: Vector[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] = ZIO.none

  override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Vector[WrapExpr], fuel: Int)
    : Comp[Option[WrapExpr]] = ZIO.none

}

object ArgonEvaluator {

  type Aux[TContext <: Context, TExprContext <: ArgonExprContext with HasContext[TContext]] =
    ArgonEvaluator {
      val context: TContext
      val exprContext: TExprContext
    }

  def apply(ctx: Context)(exprCtx: ArgonExprContext with HasContext[ctx.type])
    : ArgonEvaluator.Aux[ctx.type, exprCtx.type] =
    new ArgonEvaluator {
      override val context: ctx.type = ctx
      override val exprContext: exprCtx.type = exprCtx
    }

}
