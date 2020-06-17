package dev.argon.compiler.core

sealed trait GlobalBinding[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val name: GlobalName
}

object GlobalBinding {
  sealed trait NonNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] extends GlobalBinding[TContext, TPayloadSpec]

  final case class NestedNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName.Normal, namespace: Namespace[TContext, TPayloadSpec])
    extends GlobalBinding[TContext, TPayloadSpec]

  final case class GlobalClass[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature.ParameterOnlySignature[TContext]], arClass: ArClass.InNamespace[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalTrait[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature.ParameterOnlySignature[TContext]], arTrait: ArTrait.InNamespace[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalFunction[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature[TContext]], func: ArFunc.InNamespace[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]

  final case class GlobalDataConstructor[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (name: GlobalName, access: AccessModifierGlobal, sig: Option[ErasedSignature.ParameterOnlySignature[TContext]], dataCtor: DataConstructor.InNamespace[TContext, TPayloadSpec])
    extends NonNamespace[TContext, TPayloadSpec]


}
