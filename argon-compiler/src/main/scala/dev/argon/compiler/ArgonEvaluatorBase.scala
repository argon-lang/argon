package dev.argon.compiler

import dev.argon.util.{*, given}
import dev.argon.expr.{Evaluator, ExprContext, Substitution}
import dev.argon.util.Fuel
import zio.ZIO

private[compiler] abstract class ArgonEvaluatorBase[R, E] extends Evaluator[R, E] with UsingContext {
  override val context: Context { type Env = R; type Error = E }

  override val exprContext: context.ArgonExprContext
  protected val signatureContext: SignatureContext { val exprContext: ArgonEvaluatorBase.this.exprContext.type }

  import exprContext.{Var, Expr, ExpressionOwner}

  protected def shiftExpr(expr: context.DefaultExprContext.Expr): Expr
  protected def shiftSig(sig: context.DefaultSignatureContext.FunctionSignature): signatureContext.FunctionSignature

  override def getFunctionBody(function: ArFunc, args: Seq[Expr], fuel: Fuel): Comp[(Expr, Boolean)] =
    if function.isInline then
      function.implementation match {
        case Some(impl) =>
          impl.flatMap {
            case context.implementations.FunctionImplementation.Expr(e) =>
              val e2 = shiftExpr(e)

              for
                sig <- function.signature
              yield {
                val varMap = shiftSig(sig).parameters.zipWithIndex.map((param, index) => param.asParameterVar(ExpressionOwner.Func(function), index) : Var).zip(args).toMap
                val e3 = Substitution.substitute(exprContext)(varMap)(e2)
                (e3, true)
              }

            case context.implementations.FunctionImplementation.Extern(_) =>
              ZIO.succeed((Expr.FunctionCall(function, args), false))
          }

        case None => ZIO.succeed((Expr.FunctionCall(function, args), false))
      }
      
    else
      ZIO.succeed((Expr.FunctionCall(function, args), false))

}
