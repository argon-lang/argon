package com.mi3software.argon.compiler.core

sealed trait GlobalBinding[TContext <: Context, TPayloadSpec[_, _]] {
  val name: GlobalName
}

object GlobalBinding {
  sealed trait NonNamespace[TContext <: Context, TPayloadSpec[_, _]] extends GlobalBinding[TContext, TPayloadSpec]

  final case class NestedNamespace[TContext <: Context, TPayloadSpec[_, _]]
  (name: GlobalName, namespace: Namespace[TContext, TPayloadSpec])
    extends GlobalBinding[TContext, TPayloadSpec]

  final case class GlobalClass[TContext <: Context, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, arClass: ArClass[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalTrait[TContext <: Context, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, arTrait: ArTrait[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalFunction[TContext <: Context, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, func: ArFunc[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalDataConstructor[TContext <: Context, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, dataCtor: DataConstructor[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]


}
