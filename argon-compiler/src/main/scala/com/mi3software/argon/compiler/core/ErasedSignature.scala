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

  def fromSignature(sigContext: SignatureContext)(sig: sigContext.Signature[FunctionResultInfo]): ErasedSignature[sigContext.typeSystem.context.type] =
    sig.visit(
      sigParams => ErasedSignature.Parameter(
        TupleType(sigParams.parameter.tupleVars.map { variable => typeToSigType(sigContext)(variable.varType) }),
        fromSignature(sigContext)(sigParams.nextUnsubstituted)
      ),
      sigResult => ErasedSignature.Result(typeToSigType(sigContext)(sigResult.result.returnType))
    )

  private def typeToSigType(sigContext: SignatureContext)(t: sigContext.typeSystem.TType): SigType[sigContext.typeSystem.context.type] =
    t match {
      case t: sigContext.typeSystem.ClassType => ClassType(t.arClass)
      case t: sigContext.typeSystem.TraitType => TraitType(t.arTrait)
      case t: sigContext.typeSystem.DataConstructorType => DataConstructorType(t.ctor)
      case t: sigContext.typeSystem.TupleType => TupleType(t.elements.map { elem => typeToSigType(sigContext)(elem.elementType) })
      case t: sigContext.typeSystem.FunctionType => FunctionType(typeToSigType(sigContext)(t.argumentType), typeToSigType(sigContext)(t.resultType))
      case _ => BlankType()
    }

}
