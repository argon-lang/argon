package com.mi3software.argon.compiler

sealed trait ClassConstructor[TContext <: Context] {
  val context: TContext

  val effectInfo: EffectInfo
  val accessModifier: AccessModifier

  val instanceClass: ArClass[TContext]
}

trait ClassConstructorDeclaration[TContext <: Context] extends ClassConstructor[TContext] {
  import context._

  val instanceClass: ArClassDeclaration[TContext]
  val implementation: Comp[TClassConstructorImplementation]
}

trait ClassConstructorReference[TContext <: Context] extends ClassConstructor[TContext] {
  import context._

  val instanceClass: ArClassReference[TContext]
  val contextMetadata: TClassConstructorMetadata
}

