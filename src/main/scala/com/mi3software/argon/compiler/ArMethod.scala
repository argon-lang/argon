package com.mi3software.argon.compiler

import scalaz.Leibniz

sealed trait ArMethod[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  type PayloadSpec[_, _]

  val descriptor: MethodDescriptor

  val effectInfo: EffectInfo

  val isVirtual: Boolean
  val isAbstract: Boolean
  val isImplicitOverride: Boolean
  val isFinal: Boolean

  val signature: Signature[typeSystem.type, FunctionResultInfo]

  val payload: PayloadSpec[Comp[TMethodImplementation], TMethodMetadata]
}

trait ArMethodWithPayload[TContext <: Context, TPayloadSpec[_, _]] extends ArMethod[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}
