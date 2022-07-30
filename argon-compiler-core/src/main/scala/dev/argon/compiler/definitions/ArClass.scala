package dev.argon.compiler.definitions

import dev.argon.compiler.*
import dev.argon.compiler.module.ArModuleC
import dev.argon.util.UniqueIdentifier
import dev.argon.compiler.signature.Signature
import dev.argon.parser.IdentifierExpr

abstract class ArClassC extends Definition with UsingContext derives CanEqual {
  import context.ExprContext.*

  override val owner: ArClassC.Ownership[context.type]
  
  val id: UniqueIdentifier
  def isAbstract: Boolean
  def classMessageSource: DiagnosticSource

  def signature: Comp[Signature[WrapExpr, ClassResult]]
  def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[IsDeclaration] with HasOwner[OwnedByClass[owner.type]]]]]
  def staticMethods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[IsDeclaration] with HasOwner[OwnedByClassStatic[owner.type]]]]]
  def constructors: Comp[Seq[ClassConstructor]]
  def fields: Comp[Seq[MemberVariable]]

  type ClassResult = (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArClassC => id == other.id
      case _ => false
    }

  final override def hashCode(): Int = id.hashCode
}

object ArClassC {
  type Ownership[TContext <: Context] = OwnedByModuleC[TContext]

  def getOwningModule[TContext <: Context](owner: Ownership[TContext]): ArModuleC with HasContext[TContext] =
    owner.module
}

