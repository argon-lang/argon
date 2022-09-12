package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.signature.Signature
import zio.*

object VariableUtil {

  def getParameterVariable
  (context: Context)
  (owner: ParameterVariableOwnerC[context.type])
  (index: Int)
  : context.Comp[Option[context.ExprContext.ParameterVariable]] =
    import context.Comp
    import context.ExprContext.{WrapExpr, ParameterVariable}

    def impl(currentIndex: Int)(sig: Signature[WrapExpr, ?]): Comp[Option[ParameterVariable]] =
      sig match {
        case Signature.Parameter(_, isErased, name, paramType, _) if index == currentIndex =>
          ZIO.some(
            ParameterVariable(
              owner = owner,
              parameterIndex = index,
              varType = paramType,
              isErased = isErased,
              name = name,
            )
          )

        case Signature.Parameter(_, _, _, _, next) =>
          impl(currentIndex + 1)(next)

        case Signature.Result(_) => ZIO.none
      }

    (owner match {
      case owner: (ArClassC & HasContext[context.type]) => owner.signature
      case owner: (ArTraitC & HasContext[context.type]) => owner.signature
      case owner: (ArFuncC & HasContext[context.type]) => owner.signature
      case owner: (ArMethodC & HasContext[context.type]) => owner.signatureUnsubstituted
      case owner: (ClassConstructorC & HasContext[context.type]) => owner.signatureUnsubstituted
    } : Comp[Signature[WrapExpr, ?]]).flatMap(impl(0))
  end getParameterVariable


  def getFunctionResultVariable
  (context: Context)
  (owner: ParameterVariableOwnerC[context.type])
  : context.Comp[Option[context.ExprContext.FunctionResultVariable]] =

    import context.Comp
    import context.ExprContext.{WrapExpr, FunctionResultVariable, FunctionResult}


    (owner match {
      case owner: (ArFuncC & HasContext[context.type]) => owner.signature.asSome
      case owner: (ArMethodC & HasContext[context.type]) => owner.signatureUnsubstituted.asSome
      case _ => ZIO.none
    }: Comp[Option[Signature[WrapExpr, FunctionResult]]]).map { sig =>
      sig.map { sig =>
        val res = sig.unsubstitutedResult
        FunctionResultVariable(
          owner = owner,
          varType = res.returnType,
        )
      }
    }
  end getFunctionResultVariable
    
    
  def getInstanceVariable
  (context: Context)
  (method: ArMethodC & HasContext[context.type])
  : context.Comp[context.ExprContext.InstanceVariable] =
    import context.ExprContext.{WrapExpr, InstanceVariable}
    ???
  end getInstanceVariable


}
