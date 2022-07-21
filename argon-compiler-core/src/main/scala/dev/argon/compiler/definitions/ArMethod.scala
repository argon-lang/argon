package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArMethodC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signatureUnsubstituted: Comp[Signature[WrapExpr, WrapExpr]]

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArMethodC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}
