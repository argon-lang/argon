package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

sealed trait ArModule[TContext <: Context] {
  val context: TContext
  import context._

  val descriptor: ModuleDescriptor
  val globalNamespace: Namespace[ScopeValue[ContextScopeTypes]]
}

trait ArModuleDeclaration[TContext <: Context] extends ArModule[TContext] {
  import context._

  override val globalNamespace: Namespace[ScopeValue[DeclarationScopeTypes]]
}

trait ArModuleReference[TContext <: Context] extends ArModule[TContext] {
  import context._

  override val globalNamespace: Namespace[ScopeValue[ReferenceScopeTypes]]
}

final case class ModuleElement[+TScopeValue](namespacePath: NamespacePath, binding: NamespaceBinding[TScopeValue])
