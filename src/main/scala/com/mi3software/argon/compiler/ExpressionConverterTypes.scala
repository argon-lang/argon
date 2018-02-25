package com.mi3software.argon.compiler

import com.mi3software.argon.util.SourceLocation

trait ExpressionConverterTypes extends ExpressionConverter {

  val context: Context

  override type TExprTypes <: ArExprTypes with ({
    type TS = context.typeSystem.type
    type TFunction = ArFunc[context.type]
    type TMethod = ArMethod[context.type]
    type TClassConstructor = ClassConstructor[context.type]
  })

  override type TScopeTypes <: ScopeTypes with ({
    type TTrait = ArTrait[context.type]
    type TClass = ArClass[context.type]
    type TDataConstructor = DataConstructor[context.type]
    type TFunc = ArFunc[context.type]
    type TVariable = Variable[TS, VariableLikeDescriptor]
  })

  private class FunctionExprFactory(env: Env, location: SourceLocation)(func: TExprTypes#TFunction, args: Vector[TExprTypes#TExpr], signature: Signature[TS, FunctionResultInfo]) extends ExprFactory {
    override def withExpectedType(expectedType: TS#TType): Conv[TExprTypes#TExpr] =
      signature match {
        case parameters: SignatureParameters[TS, FunctionResultInfo] =>
          ???

        case result: SignatureResult[TS, FunctionResultInfo] =>
          fromFixedType(env, location)(
            wrapExpr(FunctionCall[TExprTypes](func, args, result.result.returnType))
          ).withExpectedType(expectedType)
      }
  }

  override protected def functionExprFactory(env: Env, location: SourceLocation)(func: ArFunc[context.type]): ExprFactory =
    new FunctionExprFactory(env, location)(func, Vector(), func.signature)
}
