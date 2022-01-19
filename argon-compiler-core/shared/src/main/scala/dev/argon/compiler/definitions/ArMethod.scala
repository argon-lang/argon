package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArMethodC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signatureUnsubstituted: Comp[Signature[WrapExpr, WrapExpr]]

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: ArMethodC => id == other.id
      case _ => false
    }

  override final def hashCode(): Int = id.hashCode
}
