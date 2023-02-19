package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class 
ClassConstructorC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  val id: UniqueIdentifier
  def signatureUnsubstituted: Comp[Signature[WrapExpr, Unit]]
  
  def purity: Boolean

  val owner: ClassConstructorC.Ownership[context.type, IsImplementation]

  type ImplementationType = IsImplementation match {
    case true => Comp[ClassConstructorImplementation]
    case false => Comp[Option[ClassConstructorImplementation]]
  }

  def implementation: ImplementationType

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ClassConstructorC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}

object ClassConstructorC {
  type Ownership[TContext <: Context, IsImplementation <: Boolean] = OwnedByClassC[TContext, ?, IsImplementation]

  def getOwningModule[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArModuleC & HasContext[TContext] & HasImplementation[IsImplementation] =
    ArClassC.getOwningModule(owner.arClass.owner)

  def getOwningTube[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArTubeC & HasContext[TContext] & HasImplementation[IsImplementation] =
    getOwningModule(owner).tube
}
