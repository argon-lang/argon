package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.{FunctionImplementationC, MethodImplementationC}
import dev.argon.expr.Evaluator
import dev.argon.util.{*, given}
import zio.*

abstract class ArgonEvaluator[R <: CompEnv, E >: CompError] extends Evaluator[R, E] with ExprUtilHoleResolver with ExprUtilCreateTypes {
  override val context: Context { type Env = R; type Error = E }
  import exprContext.{WrapExpr, ArExpr, ExprConstructor, Variable}

  final override def getFunctionBody(function: exprContext.TFunction, args: Seq[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] =
    function.maybeImplementation.flatMap {
      case Some(impl: FunctionImplementationC.ExpressionBody) if function.isInline =>
        for
          sig <- function.signature
          body = substituteBody(function)(
            convertSig(functionSigHandler)(sig),
            args.toList
          )(
            ExprToHolesConverter(context)(exprContext).processWrapExpr(impl.body)
          )
        yield Some(body)

      case _ => ZIO.none
    }

  final override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] =
    method.maybeImplementation.flatMap {
      case Some(impl: MethodImplementationC.ExpressionBody) if method.isInline && !method.isVirtual =>
        for
          sig <- method.signatureUnsubstituted
          instanceType <- createMethodInstanceType(method)
          body = substituteBody(method)(
            convertSig(functionSigHandler)(sig),
            args.toList
          )(
            substituteWrapExprMany(Map(
              exprContext.InstanceVariable(
                method,
                instanceType,
                method.instanceVariableName
              ) -> instance
            ))(
              ExprToHolesConverter(context)(exprContext).processWrapExpr(impl.body)
            )
          )
        yield Some(body)

      case _ => ZIO.none
    }

  final override def substituteVariables(varMap: Map[Variable, WrapExpr])(expr: WrapExpr): WrapExpr =
    substituteWrapExprMany(varMap)(expr)
}

object ArgonEvaluator {

  type Aux[R <: CompEnv, E >: CompError, TContext <: Context, TExprContext <: ArgonExprContext & HasContext[TContext]] =
    ArgonEvaluator[R, E] {
      val context: TContext
      val exprContext: TExprContext
    }

  def apply(ctx: Context)(exprCtx: ArgonExprContext & HasContext[ctx.type] { type THole = UniqueIdentifier })
    : ArgonEvaluator.Aux[ctx.Env, ctx.Error, ctx.type, exprCtx.type] =
    new ArgonEvaluator[ctx.Env, ctx.Error] {
      override val context: ctx.type = ctx
      override val exprContext: exprCtx.type = exprCtx
    }

}
