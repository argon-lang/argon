package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ClassConstructorC extends UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, Unit]]

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: ClassConstructorC => id == other.id
      case _ => false
    }

  override final def hashCode(): Int = id.hashCode
}
