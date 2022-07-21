package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArClassC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, ClassResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByClass[owner.type]]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByClassStatic[owner.type]]]]]
  def constructors: Comp[Seq[ClassConstructor]]

  type ClassResult = (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArClassC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}
