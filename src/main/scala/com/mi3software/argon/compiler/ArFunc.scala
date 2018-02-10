package com.mi3software.argon.compiler

sealed trait ArFunc[TContext <: Context] {
  val context: TContext

  val descriptor: FuncDescriptor
  val effectInfo: EffectInfo
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
