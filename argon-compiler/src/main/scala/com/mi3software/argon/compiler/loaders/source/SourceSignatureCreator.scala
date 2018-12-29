package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.types._
import com.mi3software.argon.parser
import com.mi3software.argon.util.WithSource
import scalaz._
import Scalaz._

object SourceSignatureCreator {

  def fromParameters[TComp[+_] : Compilation, TResult[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton]]
  (context: ContextComp[TComp])
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (paramOwner: ParameterOwnerDescriptor)
  (params: Vector[parser.FunctionParameterList])
  (resultCreator: ResultCreator[TResult])
  : TComp[context.signatureContext.Signature[TResult]] = {

    import context._
    import typeSystem.{ Variable, Parameter }
    import scopeContext.{ Scope, ScopeExtensions }
    import signatureContext.{ Signature, SignatureParameters, SignatureResult }

    def impl
    (env: ExpressionConverter.Env[context.type, Scope])
    (params: Vector[parser.FunctionParameterList])
    (paramIndex: Int)
    : TComp[Signature[TResult]] =
      params match {
        case head +: tail =>

          head.parameters
            .zipWithIndex
            .traverseU {
              case (WithSource(parser.FunctionParameter(paramTypeOpt, _, paramName), loc), tupleIndex) =>
                (paramTypeOpt match {
                  case Some(paramType) =>
                    ExpressionConverter.convertTypeExpression[TComp](context)(env)(paramType)

                  case None =>
                    Compilation[TComp].forErrors(CompilationError.ParameterTypeAnnotationRequired(paramName, CompilationMessageSource.SourceFile(env.fileSpec, loc)))

                })
                .map { t =>
                  Variable[DeconstructedParameterDescriptor](
                    DeconstructedParameterDescriptor(paramOwner, paramIndex, tupleIndex),
                    VariableName.Normal(paramName),
                    Mutability.NonMutable,
                    t
                  )
                }

            }
            .flatMap { variables: Vector[Variable[DeconstructedParameterDescriptor]] =>
              impl(
                env.copy(scope = env.scope.addVariables(variables))
              )(tail)(paramIndex + 1)
                .map { restSig =>
                  SignatureParameters[TResult](
                    Parameter(variables),
                    restSig
                  )
                }
            }

        case Vector() =>
          resultCreator.createResult(context)(env)
              .map(result => SignatureResult[TResult](result))
      }

    impl(env)(params)(0)
  }

  trait ResultCreator[TResultInfo[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton]] {
    def createResult[TComp[+_] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : TComp[TResultInfo[context.type, context.typeSystem.type]]
  }


}
