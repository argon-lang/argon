package com.mi3software.argon.compiler

trait Signature[TS <: TypeSystem, TResult[_ <: TypeSystem]] {

  def next[Comp[_]](paramType: TS#TType): Comp[Signature[TS, TResult]]

  def unsubstitutedParameters: Vector[Parameter[TS]]
  def unsubstitutedResult: TResult[TS]

  def mapResult[TNewResult[_ <: TypeSystem]](f: TResult[TS] => TNewResult[TS]): Signature[TS, TNewResult]

}


