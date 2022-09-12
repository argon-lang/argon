package dev.argon.compiler.definitions

import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.*
import dev.argon.parser.IdentifierExpr
import dev.argon.util.UniqueIdentifier

trait Definition extends DeclarationMode {
  val owner: Any
  val id: UniqueIdentifier
}

final case class OwnedByModuleC[TContext <: Context, IsImplementation <: Boolean]
(
  module: ArModuleC & HasContext[TContext] & HasImplementation[IsImplementation],
  ownedName: Option[IdentifierExpr],
  accessModifier: AccessModifierGlobal
)

final case class OwnedByClassC[TContext <: Context, +ClassOwner, IsImplementation <: Boolean]
(
  arClass: ArClassC & HasContext[TContext] & HasOwner[ClassOwner] & HasImplementation[IsImplementation],
  ownedName: Option[IdentifierExpr],
  accessModifier: AccessModifier,
)

final case class OwnedByClassStaticC[TContext <: Context, +ClassOwner, IsImplementation <: Boolean]
(
  arClass: ArClassC & HasContext[TContext] & HasOwner[ClassOwner] & HasImplementation[IsImplementation],
  ownedName: Option[IdentifierExpr],
  accessModifier: AccessModifier,
)

final case class OwnedByTraitC[TContext <: Context, +TraitOwner, IsImplementation <: Boolean]
(
  arTrait: ArTraitC & HasContext[TContext] & HasOwner[TraitOwner] & HasImplementation[IsImplementation],
  ownedName: Option[IdentifierExpr],
  accessModifier: AccessModifier,
)

final case class OwnedByTraitStaticC[TContext <: Context, +TraitOwner, IsImplementation <: Boolean]
  (
    arTrait: ArTraitC & HasContext[TContext] & HasOwner[TraitOwner] & HasImplementation[IsImplementation],
    ownedName: Option[IdentifierExpr],
    accessModifier: AccessModifier,
  )

type HasOwner[+TOwner] = Definition { val owner: TOwner }
