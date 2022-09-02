package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.module.ModuleElementC.ExportedElement
import dev.argon.parser.IdentifierExpr

enum ModuleElementC[TContext <: Context, IsImplementation <: Boolean] {
  case ClassElement(arClass: ArClassC & HasContext[TContext] & HasImplementation[IsImplementation] & HasOwner[OwnedByModuleC[TContext]])
  case TraitElement(arTrait: ArTraitC & HasContext[TContext] & HasImplementation[IsImplementation] & HasOwner[OwnedByModuleC[TContext]])
  case FunctionElement(func: ArFuncC & HasContext[TContext] & HasImplementation[IsImplementation] & HasOwner[OwnedByModuleC[TContext]])
  case ExportedElement(inner: ModuleElementC[TContext, ?])

  private def asHasOwner: HasOwner[OwnedByModuleC[TContext]] =
    this match {
      case ClassElement(e) => e
      case TraitElement(e) => e
      case FunctionElement(e) => e
      case ExportedElement(inner) => inner.asHasOwner
    }

  def name: Option[IdentifierExpr] = asHasOwner.owner.ownedName
  def module: ArModuleC & HasContext[TContext] = asHasOwner.owner.module
  def accessModifier: AccessModifierGlobal = asHasOwner.owner.accessModifier
}
