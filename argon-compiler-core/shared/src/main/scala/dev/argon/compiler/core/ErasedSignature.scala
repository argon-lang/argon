package dev.argon.compiler.core

import cats.data.NonEmptyList
import dev.argon.compiler.core.ErasedSignature.TraitType
import dev.argon.compiler.types.TypeSystem


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
    import context.signatureContext.{ context => _, typeSystem => _, _ }
    sig match {
      case SignatureParameters(parameter, nextUnsubstituted) =>
        ErasedSignature.Parameter(
          typeToSigType(context)(parameter.paramType),
          fromSignature(context)(nextUnsubstituted)
        )

      case SignatureResult(result) => ErasedSignature.Result(typeToSigType(context)(result.returnType))
    }
  }

  def fromSignatureParameters[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](context: Context)(sig: context.signatureContext.Signature[TResult, _]): ParameterOnlySignature[context.type] =
    ParameterOnlySignature(sig.unsubstitutedParameters.map { param => typeToSigType(context)(param.paramType) })

  private def typeToSigType(context: Context)(t: context.signatureContext.typeSystem.TType): SigType[context.type] =
    t match {
      case t: context.typeSystem.ClassType => ClassType(t.arClass)
      case t: context.typeSystem.TraitType => TraitType(t.arTrait)
      case t: context.typeSystem.DataConstructorType => DataConstructorType(t.ctor)
      case t: context.typeSystem.LoadTuple => TupleType(t.values.map { elem => typeToSigType(context)(elem.value) })
      case t: context.typeSystem.FunctionType => FunctionType(typeToSigType(context)(t.argumentType), typeToSigType(context)(t.resultType))
      case _ => BlankType()
    }

}
