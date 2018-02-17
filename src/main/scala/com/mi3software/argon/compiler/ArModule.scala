package com.mi3software.argon.compiler

sealed trait ArModule[TContext <: Context] {
  val context: TContext
  import context._

  val descriptor: ModuleDescriptor
  val globalNamespace: Comp[Namespace[ScopeValue[context.ContextScopeTypes]]]
}

trait ArModuleDeclaration[TContext <: Context] extends ArModule[TContext] {
  import context._

  override val globalNamespace: Comp[Namespace[ScopeValue[context.DeclarationScopeTypes]]]
}

trait ArModuleReference[TContext <: Context] extends ArModule[TContext] {
  import context._

  override val globalNamespace: Comp[Namespace[ScopeValue[context.ReferenceScopeTypes]]]
}
