package com.mi3software.argon.compiler

import com.mi3software.argon.parser
import com.mi3software.argon.util.WithSource
import ScopeHelpers._
import scalaz._
import Scalaz._


object SourceSignatureCreator {

  def fromParameters[TComp[+_] : Monad : Compilation, TResult[+_ <: TypeSystem]]
  (context: ContextComp[TComp])
  (expressionConverter: ExpressionConverterCombined[context.type])
  (env: ExpressionConvertEnvironment[expressionConverter.TScopeTypes])
  (paramOwner: ParameterOwnerDescriptor)
  (params: Vector[parser.FunctionParameterList])
  (result: TResult[context.typeSystem.type])
  : TComp[Signature[context.typeSystem.type, TResult]] = {

    def impl
    (env: ExpressionConvertEnvironment[expressionConverter.TScopeTypes])
    (params: Vector[parser.FunctionParameterList])
    (paramIndex: Int)
    : TComp[Signature[context.typeSystem.type, TResult]] =
      params match {
        case head +: tail =>

          head.parameters
            .zipWithIndex
            .traverseU {
              case (WithSource(parser.FunctionParameter(paramTypeOpt, _, paramName), loc), tupleIndex) =>
                (paramTypeOpt match {
                  case Some(paramType) =>
                    expressionConverter.convertTypeExpression(env)(paramType)
                      .eval(ExpressionConverterConv.ConvState.Default)(implicitly[Monad[TComp]])
                      .flatMap { t => HoleToArgonTypeSystemConverter(context)(expressionConverter.exprTypes.typeSystem).convertType[TComp](t) }
                      .map(identity)

                  case None =>
                    Compilation[TComp].forErrors(CompilationError.ParameterTypeAnnotationRequired(paramName, CompilationMessageSource.SourceFile(env.fileSpec, loc)))

                })
                .map { t =>
                  Variable[context.typeSystem.type, DeconstructedParameterDescriptor](
                    DeconstructedParameterDescriptor(paramOwner, paramIndex, tupleIndex),
                    VariableName.Normal(paramName),
                    Mutability.NonMutable,
                    t
                  )
                }

            }
            .flatMap { variables =>
              variables
                .traverse[TComp, Variable[expressionConverter.exprTypes.typeSystem.type, DeconstructedParameterDescriptor]] { variable =>
                  ArgonToHoleTypeSystemConverter(context)(expressionConverter.exprTypes.typeSystem).convertType[TComp](variable.varType)
                    .map { scopeVarType =>
                      Variable[expressionConverter.exprTypes.typeSystem.type, DeconstructedParameterDescriptor](
                        variable.descriptor,
                        variable.name,
                        variable.mutability,
                        scopeVarType
                      )
                    }
                }
                .flatMap { scopeVars =>
                  val env2 = env.copy(scope = env.scope.addVariables(scopeVars))

                  impl(env2)(tail)(paramIndex + 1)
                    .map { restSig =>
                      SignatureParameters[context.typeSystem.type, TResult](
                        Parameter[context.typeSystem.type](variables),
                        restSig
                      )
                    }
                }
            }

        case Vector() =>
          SignatureResult[context.typeSystem.type, TResult](result).point[TComp]
      }

    impl(env)(params)(0)
  }



}
