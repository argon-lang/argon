package dev.argon.compiler.core

import cats.data.NonEmptyList
import dev.argon.compiler.core.ErasedSignature.TraitType
import dev.argon.compiler.expr.ArExpr
import dev.argon.compiler.types.TypeSystem
import shapeless.Id


sealed trait ErasedSignature[TContext <: Context with Singleton]

object ErasedSignature {

  sealed trait SigType[TContext <: Context with Singleton]
  final case class BlankType[TContext <: Context with Singleton]() extends SigType[TContext]
  final case class TraitType[TContext <: Context with Singleton](arTrait: AbsRef[TContext, ArTrait]) extends SigType[TContext]
  final case class ClassType[TContext <: Context with Singleton](arClass: AbsRef[TContext, ArClass]) extends SigType[TContext]
  final case class DataConstructorType[TContext <: Context with Singleton](ctor: AbsRef[TContext, DataConstructor]) extends SigType[TContext]
  final case class TupleType[TContext <: Context with Singleton](elements: NonEmptyList[SigType[TContext]]) extends SigType[TContext]
  final case class FunctionType[TContext <: Context with Singleton](argumentType: SigType[TContext], resultType: SigType[TContext]) extends SigType[TContext]

  final case class Parameter[TContext <: Context with Singleton](paramType: SigType[TContext], next: ErasedSignature[TContext]) extends ErasedSignature[TContext]
  final case class Result[TContext <: Context with Singleton](resultType: SigType[TContext]) extends ErasedSignature[TContext]

  final case class ParameterOnlySignature[TContext <: Context with Singleton](paramTypes: Vector[SigType[TContext]])

  def fromSignature(context: Context)(sig: context.signatureContext.Signature[FunctionResultInfo, _]): ErasedSignature[context.type] = {
    import context.signatureContext.{ SignatureParameters, SignatureResult }
    sig match {
      case SignatureParameters(parameter, nextUnsubstituted) =>
        ErasedSignature.Parameter(
          typeToSigType(context)(parameter.paramType),
          fromSignature(context)(nextUnsubstituted)
        )

      case SignatureResult(result) =>
        ErasedSignature.Result(typeToSigType(context)(result.returnType))
    }
  }

  def fromSignatureParameters[TResult[TContext2 <: Context with Singleton, Wrap[+_]]](context: Context)(sig: context.signatureContext.Signature[TResult, _]): ParameterOnlySignature[context.type] =
    ParameterOnlySignature(sig.unsubstitutedParameters.map { param => typeToSigType(context)(param.paramType) })

  private def typeToSigType(context: Context)(t: ArExpr[context.type, Id]): SigType[context.type] =
    t match {
      case t: ArExpr.ClassType[context.type, Id] => ClassType(t.arClass)
      case t: ArExpr.TraitType[context.type, Id] => TraitType(t.arTrait)
      case t: ArExpr.DataConstructorType[context.type, Id] => DataConstructorType(t.ctor)
      case ArExpr.LoadTuple(NonEmptyList(ArExpr.TupleElement(inner), Nil)) => typeToSigType(context)(inner)
      case t: ArExpr.LoadTuple[context.type, Id] => TupleType(t.values.map { elem => typeToSigType(context)(elem.value) })
      case t: ArExpr.FunctionType[context.type, Id] => FunctionType(typeToSigType(context)(t.argumentType), typeToSigType(context)(t.resultType))
      case _ => BlankType()
    }

}
