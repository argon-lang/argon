package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArFuncC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, WrapExpr]]

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArFuncC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}
