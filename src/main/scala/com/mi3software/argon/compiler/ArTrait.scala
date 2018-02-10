package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz._
import Scalaz._

sealed trait ArTrait[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val descriptor: TraitDescriptor

  val isSealed: Boolean

  val signature: Signature[typeSystem.type, ArTrait.ResultInfo]

  val methods: Comp[Vector[ArMethod[TContext]]]
  val metaType: MetaClass[TContext, ArClass[TContext]]
}

object ArTrait {

  final case class ResultInfo[TS <: TypeSystem](baseTypes: BaseTypeInfoTrait[TS#TTraitInfo])

  implicit def equalInstance[TContext <: Context]: Equal[ArTrait[TContext]] =
    (a, b) => a.descriptor === b.descriptor

}

trait ArTraitDeclaration[TContext <: Context] extends ArTrait[TContext] {
  import context._

  val metaType: MetaClass[TContext, ArClassDeclaration[TContext]]

  val methods: Comp[Vector[ArMethodDeclaration[TContext]]]
}

trait ArTraitReference[TContext <: Context] extends ArTrait[TContext] {
  import context._

  val metaType: MetaClass[TContext, ArClassReference[TContext]]

  val methods: Comp[Vector[ArMethodReference[TContext]]]
  val contextMetadata: context.TTraitMetadata
}

trait ArTraitInNamespace[TContext <: Context] {
  self: ArTrait[TContext] =>

  override val descriptor: TraitDescriptor.InNamespace
}
