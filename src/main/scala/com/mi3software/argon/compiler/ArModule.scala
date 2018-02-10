package com.mi3software.argon.compiler

final case class ArModuleName(name: String)

sealed trait ArModule[TContext <: Context] {
  val context: TContext
  import context._

  val moduleName: ArModuleName
  val globalNamespace: Comp[Namespace[ScopeValue[TContext, ScopeValueTypesInNamespace]]]
}

trait ArModuleDeclaration[TContext <: Context] extends ArModule[TContext] {
  import context._

  override val globalNamespace: Comp[Namespace[ScopeValue[TContext, ScopeValueTypesDeclarationInNamespace]]]
}

trait ArModuleReference[TContext <: Context] extends ArModule[TContext] {
  import context._

  override val globalNamespace: Comp[Namespace[ScopeValue[TContext, ScopeValueTypesReferenceInNamespace]]]
}
