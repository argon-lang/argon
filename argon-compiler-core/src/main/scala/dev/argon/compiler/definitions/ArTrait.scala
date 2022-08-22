package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArTraitC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArTraitC.Ownership[context.type]

  val id: UniqueIdentifier
  def isSealed: Boolean
  def signature: Comp[Signature[WrapExpr, TraitResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasDeclaration[IsDeclaration] & HasOwner[OwnedByTrait[owner.type]]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasDeclaration[IsDeclaration] & HasOwner[OwnedByTraitStatic[owner.type]]]]]

  type TraitResult = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])

  // Validate inheritance rules, does not check vtables
  def validate: Comp[Unit]

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArTraitC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}

object ArTraitC {
  type Ownership[TContext <: Context] = OwnedByModuleC[TContext]

  def getOwningModule[TContext <: Context](owner: Ownership[TContext]): ArModuleC & HasContext[TContext] =
    owner.module
}
