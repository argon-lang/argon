package dev.argon.compiler.definitions

import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.*
import dev.argon.parser.IdentifierExpr
import dev.argon.util.UniqueIdentifier

trait Definition extends DeclarationMode {
  val owner: Any
  val id: UniqueIdentifier
}

final case class OwnedByModuleC[TContext <: Context]
  (module: ArModuleC with HasContext[TContext], ownedName: Option[IdentifierExpr], accessModifier: AccessModifierGlobal)

final case class OwnedByClassC[TContext <: Context, +ClassOwner]
  (
    arClass: ArClassC with HasContext[TContext] with HasOwner[ClassOwner],
    ownedName: Option[IdentifierExpr],
    accessModifier: AccessModifier,
  )

final case class OwnedByClassStaticC[TContext <: Context, +ClassOwner]
  (
    arClass: ArClassC with HasContext[TContext] with HasOwner[ClassOwner],
    ownedName: Option[IdentifierExpr],
    accessModifier: AccessModifier,
  )

final case class OwnedByTraitC[TContext <: Context, +TraitOwner]
  (
    arTrait: ArTraitC with HasContext[TContext] with HasOwner[TraitOwner],
    ownedName: Option[IdentifierExpr],
    accessModifier: AccessModifier,
  )

final case class OwnedByTraitStaticC[TContext <: Context, +TraitOwner]
  (
    arTrait: ArTraitC with HasContext[TContext] with HasOwner[TraitOwner],
    ownedName: Option[IdentifierExpr],
    accessModifier: AccessModifier,
  )

type HasOwner[+TOwner] = Definition { val owner: TOwner }
