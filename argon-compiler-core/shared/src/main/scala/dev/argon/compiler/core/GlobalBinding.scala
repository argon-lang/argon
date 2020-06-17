package dev.argon.compiler.core

import dev.argon.compiler.Comp

sealed trait GlobalBinding[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val name: GlobalName
}

object GlobalBinding {
  sealed trait NonNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] extends GlobalBinding[TContext, TPayloadSpec]

  final case class NestedNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName.Normal, namespace: Namespace[TContext, TPayloadSpec])
    extends GlobalBinding[TContext, TPayloadSpec]

  final case class GlobalClass[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature.ParameterOnlySignature[TContext]], arClass: Comp[ArClass.InNamespace[TContext, TPayloadSpec]])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalTrait[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature.ParameterOnlySignature[TContext]], arTrait: Comp[ArTrait.InNamespace[TContext, TPayloadSpec]])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalFunction[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature[TContext]], func: Comp[ArFunc.InNamespace[TContext, TPayloadSpec]])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalDataConstructor[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature.ParameterOnlySignature[TContext]], dataCtor: Comp[DataConstructor.InNamespace[TContext, TPayloadSpec]])
    extends NonNamespace[TContext, TPayloadSpec]


}
