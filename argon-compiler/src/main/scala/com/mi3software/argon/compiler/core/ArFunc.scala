package com.mi3software.argon.compiler.core

trait ArFunc[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._, signatureContext.Signature

  val descriptor: FuncDescriptor
  val effectInfo: EffectInfo

  val signature: Comp[Signature[FunctionResultInfo]]

  val payload: TPayloadSpec[Comp[TFunctionImplementation], TFunctionMetadata]
}

