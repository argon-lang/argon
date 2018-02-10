package com.mi3software.argon.compiler

sealed trait ArFunc[TContext <: Context] {
  val context: TContext
  import context._

  val descriptor: FuncDescriptor
  val effectInfo: EffectInfo

  val signature: Signature[typeSystem.type, FunctionResultInfo]
}

trait ArFuncInNamespace[TContext <: Context] {
  self: ArFunc[TContext] =>

  override val descriptor: FuncDescriptor.InNamespace
}

trait ArFuncDeclaration[TContext <: Context] extends ArFunc[TContext] {
  import context._

  val implementation: Comp[TFunctionImplementation]
}

trait ArFuncReference[TContext <: Context] extends ArFunc[TContext] {
  import context._

  val contextMetadata: TFunctionMetadata
}
