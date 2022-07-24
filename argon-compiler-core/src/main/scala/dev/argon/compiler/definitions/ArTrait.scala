package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArTraitC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, TraitResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByTrait[owner.type]]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByTraitStatic[owner.type]]]]]

  type TraitResult = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArTraitC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}