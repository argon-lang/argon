package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArClassC extends UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, ClassResult]]

  type ClassResult = (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: ArClassC => id == other.id
      case _ => false
    }

  override final def hashCode(): Int = id.hashCode
}
