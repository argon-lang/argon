package dev.argon.plugins.source

import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.compiler.signature.Signature

trait ExprUtil {
  val exprContext: ArgonExprContext
  import exprContext.{WrapExpr, ArExpr, ExprConstructor, Variable}

  def referencesVariable(variable: Variable)(expr: WrapExpr): Boolean =
    expr match {
      case WrapExpr.OfExpr(e) =>
        val hasVariableTop =
          e.constructor match {
            case ExprConstructor.LoadVariable(v) => variable == v
            case ExprConstructor.StoreVariable(v) => variable == v
            case _ => false
          }

        if hasVariableTop then
          true
        else
          e.constructor.argsToExprs(e.args).exists(referencesVariable(variable))
        end if

      case WrapExpr.OfHole(_) => false
    }

  def referencesVariableSig[Res](variable: Variable)(sigHandler: SignatureHandler[Res])(sig: Signature[WrapExpr, Res])
    : Boolean =
    sig match {
      case Signature.Parameter(_, paramType, next) =>
        referencesVariable(variable)(paramType) || referencesVariableSig(variable)(sigHandler)(next)

      case Signature.Result(res) =>
        sigHandler.resultReferences(variable)(res)
    }

  // Returns the possibly modified expression and the stable version of it
  // Declares a variable if needed
  def asStableExpression(expr: WrapExpr): exprContext.context.Comp[(WrapExpr, WrapExpr)] = ???

  def substituteWrapExpr(variable: Variable)(replacement: WrapExpr)(expr: WrapExpr): WrapExpr = ???

  def substituteClassType(variable: Variable)(replacement: WrapExpr)(expr: ArExpr[ExprConstructor.ClassType])
    : ArExpr[ExprConstructor.ClassType] = ???

  def substituteTraitType(variable: Variable)(replacement: WrapExpr)(expr: ArExpr[ExprConstructor.TraitType])
    : ArExpr[ExprConstructor.TraitType] = ???

  def substituteSignature[Res](variable: Variable)(replacement: WrapExpr)(sigHandler: SignatureHandler[Res])
    (sig: Signature[WrapExpr, Res])
    : Signature[WrapExpr, Res] =
    sig match {
      case Signature.Parameter(listType, paramType, next) =>
        Signature.Parameter(
          listType,
          substituteWrapExpr(variable)(replacement)(paramType),
          substituteSignature(variable)(replacement)(sigHandler)(next),
        )

      case Signature.Result(res) =>
        Signature.Result(sigHandler.substituteResult(variable)(replacement)(res))
    }

  trait SignatureHandler[Res] {
    def substituteResult(variable: Variable)(replacement: WrapExpr)(res: Res): Res
    def resultReferences(variable: Variable)(res: Res): Boolean
  }

}
