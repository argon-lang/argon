package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.expr.*
import dev.argon.util.{*, given}
import dev.argon.compiler.signature.Signature
import dev.argon.compiler.expr.ArgonExprContext

object EnvHelper {

  def createInnerEnv
  (exprConverter: ExpressionConverter)
  (innerEnvCell: MemoCell[exprConverter.context.Env, exprConverter.context.Error, exprConverter.Env])
  (outerEnv: exprConverter.Env)
  (owner: exprConverter.exprContext.ParameterVariableOwner)
  (signature: exprConverter.context.Comp[Signature[exprConverter.context.ExprContext.WrapExpr, ?]])
  : exprConverter.context.Comp[exprConverter.Env] =
    import exprConverter.context.ExprContext.WrapExpr
    import exprConverter.Env
    def impl(index: Int)(sig: Signature[WrapExpr, ?])(env: Env): Env =
      sig match {
        case Signature.Parameter(_, isErased, paramType, next) =>
          import exprConverter.context
          val paramType2 =
            ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprConverter.exprContext)(identity)(
              paramType
            )
          val variable = exprConverter.exprContext.ParameterVariable(owner, index, paramType2, isErased)
          impl(index + 1)(next)(env.withScope(_.addVariable(variable)))

        case Signature.Result(_) => env
      }

    innerEnvCell.get(
      for {
        sig <- signature
      } yield impl(0)(sig)(outerEnv)
    )
  end createInnerEnv

}
