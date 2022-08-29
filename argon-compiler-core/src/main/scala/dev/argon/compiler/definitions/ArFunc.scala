package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArFuncC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArFuncC.Ownership[context.type]

  val id: UniqueIdentifier
  def signature: Comp[Signature[WrapExpr, FunctionResult]]
  
  def purity: Boolean
  def isProof: Boolean
  def isErased: Boolean

  type ImplementationType = IsDeclaration match {
    case true => Comp[FunctionImplementation]
    case false => Unit
  }

  def implementation: ImplementationType

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
  type Ownership[TContext <: Context] = OwnedByModuleC[TContext]

  def getOwningModule[TContext <: Context](owner: Ownership[TContext]): ArModuleC & HasContext[TContext] =
    owner.module
}
