package com.mi3software.argon.compiler

sealed trait ClassConstructor[TContext <: Context] {
  val context: TContext
  import context._

  val effectInfo: EffectInfo
  val accessModifier: AccessModifier

  val descriptor: ClassConstructorDescriptor

  val signature: Signature[typeSystem.type, ClassConstructor.ResultInfo]

  val instanceClass: ArClass[TContext]
}

object ClassConstructor {

  final case class ResultInfo[TS <: TypeSystem]()

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

