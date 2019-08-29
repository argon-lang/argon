package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.types._
import dev.argon.parser
import dev.argon.util.WithSource
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import shapeless.Nat

object SourceSignatureCreator {

  def fromParameters[TResult[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton] : SignatureResultConverter]
  (context: Context)
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (paramOwner: ParameterOwnerDescriptor)
  (params: Vector[WithSource[parser.FunctionParameterList]])
  (resultCreator: ResultCreator[TResult])
  : context.Comp[context.signatureContext.Signature[TResult, _ <: Nat]] = {

    import context._
    import typeSystem.{ ParameterElement, Parameter, ParameterVariable }
    import scopeContext.{ Scope, ScopeExtensions }
    import signatureContext.{ Signature, SignatureParameters, SignatureResult }

    def impl
    (env: ExpressionConverter.Env[context.type, Scope])
    (params: Vector[WithSource[parser.FunctionParameterList]])
    (paramIndex: Int)
    : Comp[Signature[TResult, _ <: Nat]] =
      params match {
        case WithSource(parser.FunctionParameterList(listType, Vector()), location) +: tail =>
          ExpressionConverter.resolveUnitType(context)(env)(location)
            .flatMap { unitType =>
              impl(env)(tail)(paramIndex + 1)
                .map {
                  case restSig: Signature[TResult, len] =>
                    SignatureParameters[TResult, len](
                      Parameter(
                        ParameterStyle.fromParser(listType),
                        ParameterVariable(ParameterDescriptor(paramOwner, paramIndex), VariableName.Unnamed, Mutability.NonMutable, unitType),
                        Vector()
                      ),
                      restSig
                    )
                }
            }

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
                .map { t => (t, paramName) }
            }
            .flatMap {
              case NonEmptyList((t, name), Nil) =>

                val paramVar = ParameterVariable(ParameterDescriptor(paramOwner, paramIndex), VariableName.Normal(name), Mutability.NonMutable, t)

                impl(env.copy(scope = env.scope.addVariable(paramVar)))(tail)(paramIndex + 1)
                  .map {
                    case restSig: Signature[TResult, len] =>
                      SignatureParameters[TResult, len](
                        Parameter(
                          ParameterStyle.fromParser(listType),
                          paramVar,
                          Vector(ParameterElement(paramVar, VariableName.Normal(name), t, 0))
                        ),
                        restSig
                      )
                  }

              case elems =>

                val paramType = context.typeSystem.fromSimpleType(context.typeSystem.LoadTuple(
                  elems.map { case (t, _) => context.typeSystem.TupleElement(t) }
                ))

                val paramVar = ParameterVariable(ParameterDescriptor(paramOwner, paramIndex), VariableName.Unnamed, Mutability.NonMutable, paramType)

                val paramElems = elems.toList.toVector.zipWithIndex.map { case ((t, name), i) => ParameterElement(paramVar, VariableName.Normal(name), t, i) }

                val param = Parameter(ParameterStyle.fromParser(listType), paramVar, paramElems)

                impl(
                  env.copy(scope = env.scope.addParameter(param))
                )(tail)(paramIndex + 1)
                  .map {
                    case restSig: Signature[TResult, len] =>
                      SignatureParameters[TResult, len](
                        param,
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
