package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz._
import Scalaz._

sealed trait ArTrait[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  type PayloadSpec[_, _]

  val descriptor: TraitDescriptor

  val isSealed: Boolean

  val signature: Signature[typeSystem.type, ArTrait.ResultInfo]

  val methods: Comp[Vector[ArMethodWithPayload[TContext, PayloadSpec]]]
  val metaType: MetaClass[TContext, ArClassWithPayload[TContext, PayloadSpec]]

  val payload: PayloadSpec[Unit, TTraitMetadata]
}

object ArTrait {

  final case class ResultInfo[TS <: TypeSystem](baseTypes: BaseTypeInfoTrait[TS#TTraitInfo])

  implicit def equalInstance[TContext <: Context]: Equal[ArTrait[TContext]] =
    (a, b) => a.descriptor === b.descriptor

}

trait ArTraitWithPayload[TContext <: Context, TPayloadSpec[_, _]] extends ArTrait[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}

trait ArTraitInNamespace[TContext <: Context] {
  self: ArTrait[TContext] =>

  override val descriptor: TraitDescriptor.InNamespace
}
