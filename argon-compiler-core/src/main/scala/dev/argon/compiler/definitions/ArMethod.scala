package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArMethodC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArMethodC.Ownership[context.type, IsImplementation]

  val id: UniqueIdentifier
  def signatureUnsubstituted: Comp[Signature[WrapExpr, FunctionResult]]

  def isAbstract: Boolean
  def isImplicitOverride: Boolean
  def isVirtual: Boolean
  def isFinal: Boolean
  def isProof: Boolean
  def isErased: Boolean
  def isInline: Boolean

  def purity: Boolean

  def instanceVariableName: Option[IdentifierExpr]

  type ImplementationType = IsImplementation match {
    case true => MethodImplementation
    case false => Option[MethodImplementation]
  }

  def implementation: Comp[ImplementationType]
  def maybeImplementation: Comp[Option[MethodImplementation]]

  def validate: Comp[Unit]

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArMethodC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode

  override def toString: String =
    s"ArMethod ${owner}"
}

object ArMethodC {
  type Ownership[TContext <: Context, IsImplementation <: Boolean] =
    OwnedByClassC[TContext, ?, IsImplementation] | OwnedByClassStaticC[TContext, ?, IsImplementation] |
    OwnedByTraitC[TContext, ?, IsImplementation] | OwnedByTraitStaticC[TContext, ?, IsImplementation]

  def getOwningModule[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArModuleC & HasContext[TContext] & HasImplementation[IsImplementation] =
    owner match {
      case owner: OwnedByClassC[TContext, ?, IsImplementation] => ArClassC.getOwningModule(owner.arClass.owner)
      case owner: OwnedByClassStaticC[TContext, ?, IsImplementation] => ArClassC.getOwningModule(owner.arClass.owner)
      case owner: OwnedByTraitC[TContext, ?, IsImplementation] => ArTraitC.getOwningModule(owner.arTrait.owner)
      case owner: OwnedByTraitStaticC[TContext, ?, IsImplementation] => ArTraitC.getOwningModule(owner.arTrait .owner)
    }

  def getOwningTube[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArTubeC & HasContext[TContext] & HasImplementation[IsImplementation] =
    getOwningModule(owner).tube

  def getAccessModifier[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): AccessModifier =
    owner match {
      case owner: OwnedByClassC[TContext, ?, IsImplementation] => owner.accessModifier
      case owner: OwnedByClassStaticC[TContext, ?, IsImplementation] => owner.accessModifier
      case owner: OwnedByTraitC[TContext, ?, IsImplementation] => owner.accessModifier
      case owner: OwnedByTraitStaticC[TContext, ?, IsImplementation] => owner.accessModifier
    }
}
