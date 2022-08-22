package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ClassConstructorC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signatureUnsubstituted: Comp[Signature[WrapExpr, Unit]]
  
  val purity: Boolean

  val owner: ClassConstructorC.Ownership[context.type]

  def implementation: Comp[ClassConstructorImplementation]

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ClassConstructorC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}

object ClassConstructorC {
  type Ownership[TContext <: Context] = OwnedByClassC[TContext, ?]

  def getOwningModule[TContext <: Context](owner: Ownership[TContext]): ArModuleC & HasContext[TContext] =
    ArClassC.getOwningModule(owner.arClass.owner)
}
