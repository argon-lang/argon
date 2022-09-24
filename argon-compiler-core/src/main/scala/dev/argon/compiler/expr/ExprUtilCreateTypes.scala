package dev.argon.compiler.expr

import dev.argon.compiler.definitions.{OwnedByClassC, OwnedByClassStaticC, OwnedByTraitC, OwnedByTraitStaticC}
import dev.argon.compiler.signature.Signature

trait ExprUtilCreateTypes extends ExprUtilWithHolesBase {
  import exprContext.{
    ArExpr,
    ExprConstructor,
    Variable,
    WrapExpr,
    ClassResult,
    ParameterVariable,
    ParameterVariableOwner,
  }


  private def createTypeCommon
  (
    paramOwner: ParameterVariableOwner,
    sig: Signature[context.ExprContext.WrapExpr, ?],
    exprCtor: ExprConstructor { type ConstructorArgs = ExprConstructor.ArgList },
  ): WrapExpr =
    WrapExpr.OfExpr(
      ArExpr(
        exprCtor,
        sig.parameters.zipWithIndex.map { case (param, i) =>
          val paramType = ExprToHolesConverter(context)(exprContext).processWrapExpr(param.paramType)

          val paramVar = ParameterVariable(paramOwner, i, paramType, param.isErased, param.name)

          WrapExpr.OfExpr(
            ArExpr(
              ExprConstructor.LoadVariable(paramVar),
              EmptyTuple
            )
          )
        }
      )
    )

  def createClassType(arClass: ArClass): Comp[WrapExpr] =
    for
      sig <- arClass.signature
    yield createTypeCommon(arClass, sig, ExprConstructor.ClassType(arClass))

  def createTraitType(arTrait: ArTrait): Comp[WrapExpr] =
    for
      sig <- arTrait.signature
    yield createTypeCommon(arTrait, sig, ExprConstructor.TraitType(arTrait))

  def createMethodInstanceType(arMethod: ArMethod): Comp[WrapExpr] =
    arMethod.owner match {
      case OwnedByClassC(arClass, _, _) => createClassType(arClass)
      case OwnedByClassStaticC(arClass, _, _) => createClassType(arClass)
      case OwnedByTraitC(arTrait, _, _) => createTraitType(arTrait)
      case OwnedByTraitStaticC(arTrait, _, _) => createTraitType(arTrait)
    }
  
}
