package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.types._

trait SignatureContext extends VariableContext
{
  val typeSystem: TypeSystem

  sealed trait Signature[TResult[_ <: TypeSystem with Singleton]] {

    def unsubstitutedParameters: Vector[Parameter]
    def unsubstitutedResult: TResult[typeSystem.type]

    def convertTypeSystem(newContext: SignatureContext)(converter: TypeSystemConverter[typeSystem.type, newContext.typeSystem.type]): newContext.Signature[TResult]
    def mapResult[TNewResult[_ <: TypeSystem]](f: TResult[typeSystem.type] => TNewResult[typeSystem.type]): Signature[TNewResult]

    def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A

  }

  final case class SignatureParameters[TResult[_ <: TypeSystem with Singleton]]
  (
    parameter: Parameter,
    nextUnsubstituted: Signature[TResult]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] =
      parameter +: nextUnsubstituted.unsubstitutedParameters

    override def unsubstitutedResult: TResult[typeSystem.type] = nextUnsubstituted.unsubstitutedResult

    def next[Comp[_] : Compilation](paramType: typeSystem.TType): Comp[Signature[TResult]] = ???


    override def convertTypeSystem(newContext: SignatureContext)(converter: TypeSystemConverter[typeSystem.type, newContext.typeSystem.type]): newContext.Signature[TResult] = ???

    override def mapResult[TNewResult[_ <: TypeSystem]](f: TResult[typeSystem.type] => TNewResult[typeSystem.type]): Signature[TNewResult] = ???

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fParam(this)
  }

  final case class SignatureResult[TResult[_ <: TypeSystem with Singleton]]
  (
    result: TResult[typeSystem.type]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] = Vector.empty

    override def unsubstitutedResult: TResult[typeSystem.type] = result

    override def convertTypeSystem(newContext: SignatureContext)(converter: TypeSystemConverter[typeSystem.type, newContext.typeSystem.type]): newContext.Signature[TResult] = ???

    override def mapResult[TNewResult[_ <: TypeSystem]](f: TResult[typeSystem.type] => TNewResult[typeSystem.type]): Signature[TNewResult] = ???

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fResult(this)
  }


}
