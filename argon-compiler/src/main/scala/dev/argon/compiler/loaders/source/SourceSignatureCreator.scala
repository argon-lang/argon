package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.types._
import dev.argon.parser
import dev.argon.util.WithSource
import cats._
import cats.data.NonEmptyList
import cats.implicits._

object SourceSignatureCreator {

  def fromParameters[TResult[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton] : SignatureResultConverter]
  (context: Context)
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (paramOwner: ParameterOwnerDescriptor)
  (params: Vector[WithSource[parser.FunctionParameterList]])
  (resultCreator: ResultCreator[TResult])
  : context.Comp[context.signatureContext.Signature[TResult]] = {

    import context._
    import typeSystem.{ ParameterElementVariable, Parameter }
    import scopeContext.{ Scope, ScopeExtensions }
    import signatureContext.{ Signature, SignatureParameters, SignatureResult }

    def impl
    (env: ExpressionConverter.Env[context.type, Scope])
    (params: Vector[WithSource[parser.FunctionParameterList]])
    (paramIndex: Int)
    : Comp[Signature[TResult]] =
      params match {
        case WithSource(parser.FunctionParameterList(listType, Vector()), location) +: tail =>
          for {
            unitType <- ExpressionConverter.resolveUnitType(context)(env)(location)
            restSig <- impl(env)(tail)(paramIndex + 1)
          } yield SignatureParameters[TResult](
            Parameter(Vector(), unitType),
            restSig
          )

        case WithSource(parser.FunctionParameterList(listType, headH +: headT), location) +: tail =>

          NonEmptyList(headH, headT.toList)
            .zipWithIndex
            .traverse {
              case (WithSource(parser.FunctionParameter(paramTypeOpt, _, paramName), loc), tupleIndex) =>
                (paramTypeOpt match {
                  case Some(paramType) =>
                    ExpressionConverter.convertTypeExpression(context)(env)(paramType)

                  case None =>
                    Compilation[Comp].forErrors(CompilationError.ParameterTypeAnnotationRequired(paramName, CompilationMessageSource.SourceFile(env.fileSpec, loc)))

                })
                .map { t =>
                  ParameterElementVariable(
                    DeconstructedParameterDescriptor(paramOwner, paramIndex, tupleIndex),
                    VariableName.Normal(paramName),
                    Mutability.NonMutable,
                    t
                  )
                }

            }
            .flatMap { variables: NonEmptyList[ParameterElementVariable] =>

              val variablesVec = variables.toList.toVector

              val paramType = context.typeSystem.fromSimpleType(context.typeSystem.LoadTuple(
                variables.map { v => context.typeSystem.TupleElement(v.varType) }
              ))

              impl(
                env.copy(scope = env.scope.addVariables(variablesVec))
              )(tail)(paramIndex + 1)
                .map { restSig =>
                  SignatureParameters[TResult](
                    Parameter(variablesVec, paramType),
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
    def createResult
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : context.Comp[TResultInfo[context.type, context.typeSystem.type]]
  }


}
