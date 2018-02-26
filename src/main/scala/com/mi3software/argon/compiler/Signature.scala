package com.mi3software.argon.compiler

import com.mi3software.argon.util.Compilation

sealed trait Signature[TS <: TypeSystem, TResult[_ <: TypeSystem]] {

  def unsubstitutedParameters: Vector[Parameter[TS]]
  def unsubstitutedResult: TResult[TS]

  def convertTypeSystem[TS2 <: TypeSystem](converter: TypeSystemConverter[TS, TS2]): Signature[TS2, TResult]
  def mapResult[TNewResult[_ <: TypeSystem]](f: TResult[TS] => TNewResult[TS]): Signature[TS, TNewResult]

}

trait SignatureParameters[TS <: TypeSystem, TResult[_ <: TypeSystem]] extends Signature[TS, TResult] {

  val parameter: Parameter[TS]
  def next[Comp[_] : Compilation](paramType: TS#TType): Comp[Signature[TS, TResult]]

}

trait SignatureResult[TS <: TypeSystem, TResult[_ <: TypeSystem]] extends Signature[TS, TResult] {

  val result: TResult[TS]

}
