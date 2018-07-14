package com.mi3software.argon.compiler

import com.mi3software.argon.util.Compilation

sealed trait Signature[+TS <: TypeSystem, TResult[+_ <: TypeSystem]] {

  def unsubstitutedParameters: Vector[Parameter[TS]]
  def unsubstitutedResult: TResult[TS]

  def convertTypeSystem[TS1 >: TS <: TypeSystem, TS2 <: TypeSystem](converter: TypeSystemConverter[TS1, TS2]): Signature[TS2, TResult]
  def mapResult[TS1 >: TS <: TypeSystem, TNewResult[+_ <: TypeSystem]](f: TResult[TS] => TNewResult[TS1]): Signature[TS1, TNewResult]

}

final case class SignatureParameters[+TS <: TypeSystem, TResult[+_ <: TypeSystem]]
(
  parameter: Parameter[TS],
  nextUnsubstituted: Signature[TS, TResult]
) extends Signature[TS, TResult] {

  override def unsubstitutedParameters: Vector[Parameter[TS]] =
    parameter +: nextUnsubstituted.unsubstitutedParameters

  override def unsubstitutedResult: TResult[TS] = nextUnsubstituted.unsubstitutedResult

  def next[TS1 >: TS <: TypeSystem, Comp[_] : Compilation](paramType: TS1#TType): Comp[Signature[TS1, TResult]] = ???

  override def convertTypeSystem[TS1 >: TS <: TypeSystem, TS2 <: TypeSystem](converter: TypeSystemConverter[TS1, TS2]): Signature[TS2, TResult] = ???

  override def mapResult[TS1 >: TS <: TypeSystem, TNewResult[+_ <: TypeSystem]](f: TResult[TS] => TNewResult[TS1]): Signature[TS1, TNewResult] = ???
}

final case class SignatureResult[+TS <: TypeSystem, TResult[+_ <: TypeSystem]]
(
  result: TResult[TS]
) extends Signature[TS, TResult] {

  override def unsubstitutedParameters: Vector[Parameter[TS]] = Vector.empty

  override def unsubstitutedResult: TResult[TS] = result

  override def convertTypeSystem[TS1 >: TS <: TypeSystem, TS2 <: TypeSystem](converter: TypeSystemConverter[TS1, TS2]): Signature[TS2, TResult] = ???

  override def mapResult[TS1 >: TS <: TypeSystem, TNewResult[+_ <: TypeSystem]](f: TResult[TS] => TNewResult[TS1]): Signature[TS1, TNewResult] = ???
}
