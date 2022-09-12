package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.compiler.tube.ArTubeC
import dev.argon.parser.IdentifierExpr

abstract class ArTraitC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArTraitC.Ownership[context.type, IsImplementation]

  val id: UniqueIdentifier
  def isSealed: Boolean
  def signature: Comp[Signature[WrapExpr, TraitResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByTrait[owner.type, IsImplementation]]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByTraitStatic[owner.type, IsImplementation]]]]]

  def vtable: Comp[context.VT.VTable]

  // Validate inheritance rules, does not check vtables
  def validate: Comp[Unit]

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArTraitC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode

  override def toString: String =
    s"ArTrait(${owner.module.moduleName}/${owner.ownedName})"
}

object ArTraitC {
  type Ownership[TContext <: Context, IsImplementation <: Boolean] = OwnedByModuleC[TContext, IsImplementation]

  def getOwningModule[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArModuleC & HasContext[TContext] & HasImplementation[IsImplementation] =
    owner.module

  def getOwningTube[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArTubeC & HasContext[TContext] & HasImplementation[IsImplementation] =
    getOwningModule(owner).tube
}
