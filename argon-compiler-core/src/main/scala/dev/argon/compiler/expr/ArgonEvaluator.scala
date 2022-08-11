package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.expr.Evaluator
import zio.*

abstract class ArgonEvaluator[R <: CompEnv, E >: CompError] extends UsingContext with Evaluator[R, E] {
  override val context: Context { type Env = R; type Error = E }
  override val exprContext: ArgonExprContext & HasContext[context.type]
  import exprContext.WrapExpr

  override def getFunctionBody(function: ArFunc, args: Seq[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] = ZIO.none

  override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int)
    : Comp[Option[WrapExpr]] = ZIO.none

}

object ArgonEvaluator {

  type Aux[R <: CompEnv, E >: CompError, TContext <: Context, TExprContext <: ArgonExprContext & HasContext[TContext]] =
    ArgonEvaluator[R, E] {
      val context: TContext
      val exprContext: TExprContext
    }

  def apply(ctx: Context)(exprCtx: ArgonExprContext & HasContext[ctx.type])
    : ArgonEvaluator.Aux[ctx.Env, ctx.Error, ctx.type, exprCtx.type] =
    new ArgonEvaluator[ctx.Env, ctx.Error] {
      override val context: ctx.type = ctx
      override val exprContext: exprCtx.type = exprCtx
    }

}
