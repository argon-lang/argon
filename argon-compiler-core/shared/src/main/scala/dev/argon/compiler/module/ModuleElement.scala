package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.parser.IdentifierExpr

enum ModuleElementC[TContext <: Context] {
  case ClassElement(arClass: ArClassC with HasContext[TContext] with HasOwner[OwnedByModuleC[TContext]])
  case TraitElement(arTrait: ArTraitC with HasContext[TContext] with HasOwner[OwnedByModuleC[TContext]])
  case FunctionElement(func: ArFuncC with HasContext[TContext] with HasOwner[OwnedByModuleC[TContext]])

  private def asHasOwner: HasOwner[OwnedByModuleC[TContext]] =
    this match {
      case ClassElement(e) => e
      case TraitElement(e) => e
      case FunctionElement(e) => e
    }

  def name: Option[IdentifierExpr] = asHasOwner.owner.ownedName
}
