package dev.argon.compiler.core

import cats.Eq
import cats.data.NonEmptyList
import dev.argon.compiler.expr.ArExpr
import cats.implicits._
import dev.argon.util.{DeriveHelpers, Id}

sealed trait ErasedSignature[TContext <: Context with Singleton]

object ErasedSignature {

  implicit def erasedSignatureEqInstance[TContext <: Context with Singleton]: Eq[ErasedSignature[TContext]] = DeriveHelpers.eq

  sealed trait SigType[TContext <: Context with Singleton]
  object SigType {
    implicit def eqInstance[TContext <: Context with Singleton]: Eq[SigType[TContext]] = DeriveHelpers.eq
  }

  final case class BlankType[TContext <: Context with Singleton]() extends SigType[TContext]

  final case class TraitType[TContext <: Context with Singleton](arTrait: AbsRef[TContext, ArTrait], typeArgs: Vector[SigType[TContext]]) extends SigType[TContext]
  object TraitType {
    implicit def eqInstance[TContext <: Context with Singleton]: Eq[TraitType[TContext]] = (a, b) =>
      a.arTrait.value.id === b.arTrait.value.id && a.typeArgs === b.typeArgs
  }

  final case class ClassType[TContext <: Context with Singleton](arClass: AbsRef[TContext, ArClass], typeArgs: Vector[SigType[TContext]]) extends SigType[TContext]
  object ClassType {
    implicit def eqInstance[TContext <: Context with Singleton]: Eq[ClassType[TContext]] = (a, b) =>
      a.arClass.value.id === b.arClass.value.id && a.typeArgs === b.typeArgs
  }

  final case class DataConstructorType[TContext <: Context with Singleton](ctor: AbsRef[TContext, DataConstructor], typeArgs: Vector[SigType[TContext]]) extends SigType[TContext]
  object DataConstructorType {
    implicit def eqInstance[TContext <: Context with Singleton]: Eq[DataConstructorType[TContext]] = (a, b) =>
      a.ctor.value.id === b.ctor.value.id && a.typeArgs === b.typeArgs
  }

  final case class TupleType[TContext <: Context with Singleton](elements: NonEmptyList[SigType[TContext]]) extends SigType[TContext]
  final case class FunctionType[TContext <: Context with Singleton](argumentType: SigType[TContext], resultType: SigType[TContext]) extends SigType[TContext]

  final case class Parameter[TContext <: Context with Singleton](paramType: SigType[TContext], next: ErasedSignature[TContext]) extends ErasedSignature[TContext]
  final case class Result[TContext <: Context with Singleton](resultType: SigType[TContext]) extends ErasedSignature[TContext]

  final case class ParameterOnlySignature[TContext <: Context with Singleton](paramTypes: Vector[SigType[TContext]])

  object ParameterOnlySignature {
    implicit def eqInstance[TContext <: Context with Singleton]: Eq[ParameterOnlySignature[TContext]] = DeriveHelpers.eq

  }

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
    ParameterOnlySignature(sig.unsubstitutedParameters.toVector.map { param => typeToSigType(context)(param.paramType) })

  private def typeToSigType(context: Context)(t: ArExpr[context.type, Id]): SigType[context.type] =
    t match {
      case t: ArExpr.ClassType[context.type, Id] => ClassType(t.arClass, t.args.map(typeToSigType(context)(_)))
      case t: ArExpr.TraitType[context.type, Id] => TraitType(t.arTrait, t.args.map(typeToSigType(context)(_)))
      case t: ArExpr.DataConstructorType[context.type, Id] => DataConstructorType(t.ctor, t.args.map(typeToSigType(context)(_)))
      case ArExpr.LoadTuple(NonEmptyList(ArExpr.TupleElement(inner), Nil)) => typeToSigType(context)(inner)
      case t: ArExpr.LoadTuple[context.type, Id] => TupleType(t.values.map { elem => typeToSigType(context)(elem.value) })
      case t: ArExpr.FunctionType[context.type, Id] => FunctionType(typeToSigType(context)(t.argumentType), typeToSigType(context)(t.resultType))
      case _ => BlankType()
    }

}
