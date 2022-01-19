package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArTraitC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, TraitResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByTrait]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByTraitStatic]]]]

  type TraitResult = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: ArTraitC => id == other.id
      case _ => false
    }

  override final def hashCode(): Int = id.hashCode
}
