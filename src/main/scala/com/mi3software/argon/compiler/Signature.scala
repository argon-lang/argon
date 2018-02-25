package com.mi3software.argon.compiler

sealed trait Signature[TS <: TypeSystem, TResult[_ <: TypeSystem]] {

  def unsubstitutedParameters: Vector[Parameter[TS]]
  def unsubstitutedResult: TResult[TS]

  def mapResult[TNewResult[_ <: TypeSystem]](f: TResult[TS] => TNewResult[TS]): Signature[TS, TNewResult]

}

trait SignatureParameters[TS <: TypeSystem, TResult[_ <: TypeSystem]] extends Signature[TS, TResult] {

  def next[Comp[_]](paramType: TS#TType): Comp[Signature[TS, TResult]]

}

trait SignatureResult[TS <: TypeSystem, TResult[_ <: TypeSystem]] extends Signature[TS, TResult] {

  val result: TResult[TS]

}
