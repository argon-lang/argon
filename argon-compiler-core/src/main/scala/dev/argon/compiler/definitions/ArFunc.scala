package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArFuncC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArFuncC.Ownership[context.type, IsImplementation]

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, FunctionResult]]
  
  def purity: Boolean
  def isProof: Boolean
  def isErased: Boolean
  def isInline: Boolean

  type ImplementationType = IsImplementation match {
    case true => Comp[FunctionImplementation]
    case false => Comp[Option[FunctionImplementation]]
  }

  def implementation: ImplementationType
  def maybeImplementation: Comp[Option[FunctionImplementation]]

  def validate: Comp[Unit]

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArFuncC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode

  override def toString: String =
    s"ArFunc ${owner}"
}

object ArFuncC {
  type Ownership[TContext <: Context, IsImplementation <: Boolean] = OwnedByModuleC[TContext, IsImplementation]

  def getOwningModule[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArModuleC & HasContext[TContext] & HasImplementation[IsImplementation] =
    owner.module

  def getOwningTube[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArTubeC & HasContext[TContext] & HasImplementation[IsImplementation] =
    getOwningModule(owner).tube
}
