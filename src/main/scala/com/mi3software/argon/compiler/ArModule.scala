package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

sealed trait ArModule[+TContext <: Context] {
  val context: TContext
  import context._

  type PayloadSpec[_, _]

  val descriptor: ModuleDescriptor
  val globalNamespace: Namespace[ScopeValue[ContextScopeTypes]]
}



trait ArModuleWithPayload[+TContext <: Context, TPayloadSpec[_, _]] extends ArModule[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}

final case class ModuleElement[+TScopeValue](namespacePath: NamespacePath, binding: NamespaceBinding[TScopeValue])
