package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.expr.Evaluator
import dev.argon.util.{*, given}
import zio.*

abstract class ArgonEvaluator[R <: CompEnv, E >: CompError] extends Evaluator[R, E] with ExprUtilSubstitution {
  override val context: Context { type Env = R; type Error = E }
  import exprContext.{WrapExpr, ArExpr, ExprConstructor, Variable}

  final override def getFunctionBody(function: exprContext.TFunction, args: Seq[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] = ZIO.none

  final override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int)
    : Comp[Option[WrapExpr]] = ZIO.none

  final override def substituteVariables(varMap: Map[Variable, WrapExpr])(expr: WrapExpr): WrapExpr =
    substituteWrapExprMany(varMap)(expr)
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
