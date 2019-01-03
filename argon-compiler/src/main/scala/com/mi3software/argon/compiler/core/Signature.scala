package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.types._

trait SignatureContext[TContext <: Context with Singleton]
{
  val context: TContext
  val typeSystem: TypeSystem[context.type]
  import typeSystem.Parameter

  sealed trait Signature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]] {

    def unsubstitutedParameters: Vector[Parameter]
    def unsubstitutedResult: TResult[context.type, typeSystem.type]

    def convertTypeSystem(newContext: SignatureContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type]): newContext.Signature[TResult]
    def mapResult[TNewResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2]]](f: TResult[context.type, typeSystem.type] => TNewResult[context.type, typeSystem.type]): Signature[TNewResult]

    def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A

  }

  final case class SignatureParameters[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (
    parameter: Parameter,
    nextUnsubstituted: Signature[TResult]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] =
      parameter +: nextUnsubstituted.unsubstitutedParameters

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = nextUnsubstituted.unsubstitutedResult

    def next[Comp[_] : Compilation](paramType: typeSystem.TType): Comp[Signature[TResult]] = ???

    override def convertTypeSystem(newContext: SignatureContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type]): newContext.Signature[TResult] =
      newContext.SignatureParameters(
        TypeSystem.convertParameterTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(parameter),
        nextUnsubstituted.convertTypeSystem(newContext)(converter)
      )

    override def mapResult[TNewResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2]]](f: TResult[context.type, typeSystem.type] => TNewResult[context.type, typeSystem.type]): Signature[TNewResult] = ???

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fParam(this)
  }

  final case class SignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton] : SignatureResultConverter]
  (
    result: TResult[context.type, typeSystem.type]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] = Vector.empty

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = result

    override def convertTypeSystem(newContext: SignatureContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type]): newContext.Signature[TResult] =
      newContext.SignatureResult(
        implicitly[SignatureResultConverter[TResult]].convertTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(result)
      )


    override def mapResult[TNewResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2]]](f: TResult[context.type, typeSystem.type] => TNewResult[context.type, typeSystem.type]): Signature[TNewResult] = ???

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fResult(this)
  }


}
