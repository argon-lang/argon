package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArClassC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArClassC.Ownership[context.type, IsImplementation]
  
  val id: UniqueIdentifier
  def isAbstract: Boolean
  def isSealed: Boolean
  def isOpen: Boolean
  def classMessageSource: DiagnosticSource

  def signature: Comp[Signature[WrapExpr, ClassResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByClass[owner.type, IsImplementation]]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[IsImplementation] & HasOwner[OwnedByClassStatic[owner.type, IsImplementation]]]]]
  def constructors: Comp[Seq[ClassConstructor & HasImplementation[IsImplementation]]]
  def fields: Comp[Seq[MemberVariable]]

  def vtable: Comp[context.VT.VTable]
  def vtableDiff: Comp[context.VT.VTable]
  
  // Validate inheritance rules, does not check vtables
  def validate: Comp[Unit]

  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArClassC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode

  override def toString: String =
    s"ArClass(${owner.module.moduleName}/${owner.ownedName})"
}

object ArClassC {
  type Ownership[TContext <: Context, IsImplementation <: Boolean] = OwnedByModuleC[TContext, IsImplementation]

  def getOwningModule[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArModuleC & HasContext[TContext] & HasImplementation[IsImplementation] =
    owner.module

  def getOwningTube[TContext <: Context, IsImplementation <: Boolean](owner: Ownership[TContext, IsImplementation]): ArTubeC & HasContext[TContext] & HasImplementation[IsImplementation] =
    getOwningModule(owner).tube
}

