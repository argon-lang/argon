package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ClassConstructorC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, Unit]]

  val owner: (ArClass, AccessModifier)

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ClassConstructorC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}
