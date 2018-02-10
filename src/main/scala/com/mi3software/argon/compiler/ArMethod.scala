package com.mi3software.argon.compiler

import scalaz.Leibniz

sealed trait ArMethod[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val descriptor: MethodDescriptor

  val effectInfo: EffectInfo

  val isVirtual: Boolean
  val isAbstract: Boolean
  val isImplicitOverride: Boolean
  val isFinal: Boolean

  val signature: Signature[typeSystem.type, FunctionResultInfo]

}

trait ArMethodDeclaration[TContext <: Context] extends ArMethod[TContext] {
  import context._

  val implementation: Comp[context.TMethodImplementation]
}

trait ArMethodReference[TContext <: Context] extends ArMethod[TContext] {
  import context._

  val contextMetadata: TMethodMetadata
}
