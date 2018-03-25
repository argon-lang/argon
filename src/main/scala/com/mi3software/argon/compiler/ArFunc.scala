package com.mi3software.argon.compiler

sealed trait ArFunc[TContext <: Context] {
  val context: TContext
  import context._

  type PayloadSpec[_, _]

  val descriptor: FuncDescriptor
  val effectInfo: EffectInfo

  val signature: Signature[typeSystem.type, FunctionResultInfo]

  val payload: PayloadSpec[Comp[TFunctionImplementation], TFunctionMetadata]
}

trait ArFuncInNamespace[TContext <: Context] {
  self: ArFunc[TContext] =>

  override val descriptor: FuncDescriptor.InNamespace
}

trait ArFuncWithPayload[TContext <: Context, TPayloadSpec[_, _]] extends ArFunc[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}
