package com.mi3software.argon.compiler

import com.mi3software.argon.util.SourceLocation

import scalaz._
import Scalaz._

trait ExpressionConverterTypes extends ExpressionConverter {

  val context: Context

  override type TExprTypes <: ArExprTypes with ({
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

  protected val contextTypeSystemConverter: TypeSystemConverter[context.typeSystem.type, TS]

  private class FunctionExprFactory(env: Env, location: SourceLocation)(func: TExprTypes#TFunction, args: Vector[TExprTypes#TExpr], signature: Signature[TS, FunctionResultInfo]) extends ExprFactory {


    override def forArguments(argInfo: ArgumentInfo): ExprFactory =
      signature match {
        case parameters: SignatureParameters[TS, FunctionResultInfo] =>
          fromConv(
            argInfo.argFactory.withExpectedType(parameters.parameter.paramType).flatMap { param =>
              convertExpressionToType(param)
                .flatMap { paramAsType =>
                  parameters.next(paramAsType)
                }
                .map { sigNext =>
                  new FunctionExprFactory(env, argInfo.location)(func, args :+ param, sigNext)
                }
            }
          )

        case result: SignatureResult[TS, FunctionResultInfo] =>
          fromFixedType(env, location)(
            wrapExpr(FunctionCall[TExprTypes](func, args, result.result.returnType))
          ).forArguments(argInfo)
      }

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
    new FunctionExprFactory(env, location)(func, Vector(), func.signature.convertTypeSystem(contextTypeSystemConverter))
}
