package com.mi3software.argon.compiler.core

sealed trait GlobalBinding[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val name: GlobalName
}

object GlobalBinding {
  sealed trait NonNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] extends GlobalBinding[TContext, TPayloadSpec]

  final case class NestedNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, namespace: Namespace[TContext, TPayloadSpec])
    extends GlobalBinding[TContext, TPayloadSpec]

  final case class GlobalClass[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, arClass: ArClass[TContext, TPayloadSpec] { val descriptor: ClassDescriptor.InNamespace })
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalTrait[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, arTrait: ArTrait[TContext, TPayloadSpec] { val descriptor: TraitDescriptor.InNamespace })
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalFunction[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, func: ArFunc[TContext, TPayloadSpec] { val descriptor: FuncDescriptor.InNamespace })
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalDataConstructor[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, dataCtor: DataConstructor[TContext, TPayloadSpec] { val descriptor: DataConstructorDescriptor.InNamespace })
    extends NonNamespace[TContext, TPayloadSpec]


}
