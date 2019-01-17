package com.mi3software.argon.compiler.core

import scalaz.Leibniz

sealed trait ArMethod[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._, signatureContext.Signature

  val descriptor: MethodDescriptor

  val effectInfo: EffectInfo

  val isVirtual: Boolean
  val isAbstract: Boolean
  val isImplicitOverride: Boolean
  val isFinal: Boolean

  val signature: Comp[Signature[FunctionResultInfo]]

  val payload: TPayloadSpec[Comp[TMethodImplementation], TMethodMetadata]
}

