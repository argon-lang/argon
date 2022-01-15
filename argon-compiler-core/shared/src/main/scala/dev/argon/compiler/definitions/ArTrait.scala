package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArTraitC extends UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, TraitResult]]

  type TraitResult = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: ArTraitC => id == other.id
      case _ => false
    }

  override final def hashCode(): Int = id.hashCode
}
