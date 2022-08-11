package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature

abstract class ArMethodC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArMethodC.Ownership[context.type]

  val id: UniqueIdentifier
  def signatureUnsubstituted: Comp[Signature[WrapExpr, WrapExpr]]

  def isAbstract: Boolean
  def isImplicitOverride: Boolean
  def isVirtual: Boolean
  def isFinal: Boolean

  type ImplementationType = IsDeclaration match {
    case true => Comp[MethodImplementation]
    case false => Unit
  }

  def implementation: ImplementationType


  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArMethodC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}

object ArMethodC {
  type Ownership[TContext <: Context] =
    OwnedByClassC[TContext, ?] | OwnedByClassStaticC[TContext, ?] |
    OwnedByTraitC[TContext, ?] | OwnedByTraitStaticC[TContext, ?]

  def getOwningModule[TContext <: Context](owner: Ownership[TContext]): ArModuleC & HasContext[TContext] =
    owner match {
      case owner: OwnedByClassC[TContext, ?] => ArClassC.getOwningModule(owner.arClass.owner)
      case owner: OwnedByClassStaticC[TContext, ?] => ArClassC.getOwningModule(owner.arClass.owner)
      case owner: OwnedByTraitC[TContext, ?] => ArTraitC.getOwningModule(owner.arTrait .owner)
      case owner: OwnedByTraitStaticC[TContext, ?] => ArTraitC.getOwningModule(owner.arTrait .owner)
    }
}
