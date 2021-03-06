package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.parser
import dev.argon.util.{Id, Nat, VectorUnCons, WithSource}
import cats.data.NonEmptyList
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import zio.interop.catz.core._

object SourceSignatureCreator {

  def fromParameters[TResult[TContext <: Context with Singleton, Wrap[+_]] : SignatureResultConverter]
  (context: Context)
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (paramOwner: ParameterVariableOwner[context.type])
  (params: Vector[WithSource[parser.FunctionParameterList]])
  (resultCreator: ResultCreator.Aux[context.type, TResult])
  : Comp[context.signatureContext.Signature[TResult, _ <: Nat]] = {

    import context._
    import scopeContext.{ Scope, ScopeExtensions }
    import signatureContext.{ Signature, SignatureParameters, SignatureResult }

    def impl
    (env: ExpressionConverter.Env[context.type, Scope])
    (params: Vector[WithSource[parser.FunctionParameterList]])
    (paramIndex: Int)
    : Comp[Signature[TResult, _ <: Nat]] =
      params match {
        case VectorUnCons(VectorUnCons.NonEmpty(WithSource(parser.FunctionParameterList(listType, isErased, VectorUnCons(VectorUnCons.Empty)), location), tail)) =>
          ExpressionConverter.resolveUnitType(context)(env)(location)
            .flatMap { unitType =>
              impl(env)(tail)(paramIndex + 1)
                .map {
                  case restSig: Signature[TResult, len] =>
                    SignatureParameters[TResult, len](
                      Parameter(
                        ParameterStyle.fromParser(listType),
                        ParameterVariable[context.type, Id](paramOwner, paramIndex, VariableName.Unnamed, Mutability.NonMutable, isErased = isErased, unitType),
                        Vector()
                      ),
                      restSig
                    )
                }
            }

        case VectorUnCons(VectorUnCons.NonEmpty(WithSource(parser.FunctionParameterList(listType, isErased, VectorUnCons(VectorUnCons.NonEmpty(headH, headT))), _), tail)) =>
          NonEmptyList(headH, headT.toList)
            .traverse {
              case WithSource(parser.FunctionParameter(paramTypeOpt, _, paramName), loc) =>
                (paramTypeOpt match {
                  case Some(paramType) =>
                    ExpressionConverter.convertTypeExpression(context)(env)(paramType)

                  case None =>
                    Compilation.forErrors(DiagnosticError.ParameterTypeAnnotationRequired(paramName, DiagnosticSource.SourceFile(env.fileSpec, loc)))

                })
                .map { t => (t, paramName) }
            }
            .flatMap {
              case NonEmptyList((t, name), Nil) =>

                val paramVar = ParameterVariable[context.type, Id](paramOwner, paramIndex, VariableName.Normal(name), Mutability.NonMutable, isErased = isErased, t)

                impl(env.copy(scope = env.scope.addVariable(paramVar)))(tail)(paramIndex + 1)
                  .map {
                    case restSig: Signature[TResult, len] =>
                      SignatureParameters[TResult, len](
                        Parameter(
                          ParameterStyle.fromParser(listType),
                          paramVar,
                          Vector(ParameterElement[context.type, Id](paramVar, VariableName.Normal(name), t, 0))
                        ),
                        restSig
                      )
                  }

              case elems =>

                val paramType = context.typeSystem.fromSimpleType(LoadTuple(
                  elems.map { case (t, _) => TupleElement[context.type, Id](t) }
                ))

                val paramVar = ParameterVariable[context.type, Id](paramOwner, paramIndex, VariableName.Unnamed, Mutability.NonMutable, isErased = isErased, paramType)

                val paramElems = elems.toList.toVector.zipWithIndex.map { case ((t, name), i) => ParameterElement[context.type, Id](paramVar, VariableName.Normal(name), t, i) }

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

        case VectorUnCons(VectorUnCons.Empty) =>
          resultCreator.createResult(env)
              .map(result => SignatureResult[TResult](result))
      }

    impl(env)(params)(0)
  }

  trait ResultCreator[TResultInfo[TContext <: Context with Singleton, Wrap[+_]]] {
    val context: Context

    def createResult
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : Comp[TResultInfo[context.type, context.typeSystem.TTypeWrapper]]
  }

  object ResultCreator {

    type Aux[TContext <: Context with Singleton, TResultInfo[_ <: Context with Singleton, Wrap[+_]]] =
      ResultCreator[TResultInfo] {
        val context: TContext
      }

  }


}
