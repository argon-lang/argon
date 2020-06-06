package dev.argon.compiler.core

sealed trait AccessToken
object AccessToken {
  final case class OfClass[TContext <: Context with Singleton](arClass: AbsRef[TContext, ArClass]) extends AccessToken
  final case class OfTrait[TContext <: Context with Singleton](arTrait: AbsRef[TContext, ArTrait]) extends AccessToken
  final case class OfDataConstructor[TContext <: Context with Singleton](ctor: AbsRef[TContext, DataConstructor]) extends AccessToken
}
