package dev.argon.compiler.definitions

import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.*
import dev.argon.parser.IdentifierExpr


trait Definition {
  val owner: Any
}

final case class OwnedByModuleC[TContext <: Context](module: ArModuleC with HasContext[TContext], ownedName: Option[IdentifierExpr], accessModifier: AccessModifierGlobal)
final case class OwnedByClassC[TContext <: Context](arClass: ArClassC with HasContext[TContext], ownedName: Option[IdentifierExpr], accessModifier: AccessModifierGlobal)
final case class OwnedByClassStaticC[TContext <: Context](arClass: ArClassC with HasContext[TContext], ownedName: Option[IdentifierExpr], accessModifier: AccessModifierGlobal)
final case class OwnedByTraitC[TContext <: Context](arTrait: ArTraitC with HasContext[TContext], ownedName: Option[IdentifierExpr], accessModifier: AccessModifierGlobal)
final case class OwnedByTraitStaticC[TContext <: Context](arTrait: ArTraitC with HasContext[TContext], ownedName: Option[IdentifierExpr], accessModifier: AccessModifierGlobal)

type HasOwner[TOwner] = Definition { val owner: TOwner }

 

