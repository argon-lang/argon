package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.core.ErasedSignature.TraitType
import com.mi3software.argon.compiler.types.TypeSystem

sealed trait ErasedSignature[TContext <: Context]

object ErasedSignature {

  sealed trait SigType[TContext <: Context]
  final case class BlankType[TContext <: Context]() extends SigType[TContext]
  final case class TraitType[TContext <: Context](arTrait: AbsRef[TContext, ArTrait]) extends SigType[TContext]
  final case class ClassType[TContext <: Context](arClass: AbsRef[TContext, ArClass]) extends SigType[TContext]
  final case class DataConstructorType[TContext <: Context](ctor: AbsRef[TContext, DataConstructor]) extends SigType[TContext]
  final case class TupleType[TContext <: Context](elements: Vector[SigType[TContext]]) extends SigType[TContext]
  final case class FunctionType[TContext <: Context](argumentType: SigType[TContext], resultType: SigType[TContext]) extends SigType[TContext]

  final case class Parameter[TContext <: Context](paramType: SigType[TContext], next: ErasedSignature[TContext]) extends ErasedSignature[TContext]
  final case class Result[TContext <: Context](resultType: SigType[TContext]) extends ErasedSignature[TContext]

  def fromSignature(context: Context)(sig: context.signatureContext.Signature[FunctionResultInfo]): ErasedSignature[context.type] =
    sig.visit(
      sigParams => ErasedSignature.Parameter(
        TupleType(sigParams.parameter.tupleVars.map { variable => typeToSigType(context)(variable.varType) }),
        fromSignature(context)(sigParams.nextUnsubstituted)
      ),
      sigResult => ErasedSignature.Result(typeToSigType(context)(sigResult.result.returnType))
    )

  private def typeToSigType(context: Context)(t: context.signatureContext.typeSystem.TType): SigType[context.type] =
    t match {
      case t: context.typeSystem.ClassType => ClassType(t.arClass)
      case t: context.typeSystem.TraitType => TraitType(t.arTrait)
      case t: context.typeSystem.DataConstructorType => DataConstructorType(t.ctor)
      case t: context.typeSystem.TupleType => TupleType(t.elements.map { elem => typeToSigType(context)(elem.elementType) })
      case t: context.typeSystem.FunctionType => FunctionType(typeToSigType(context)(t.argumentType), typeToSigType(context)(t.resultType))
      case _ => BlankType()
    }

}
