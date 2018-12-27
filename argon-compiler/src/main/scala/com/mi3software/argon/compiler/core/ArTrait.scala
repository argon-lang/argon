package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.types._
import scalaz.Scalaz._
import scalaz._

import scala.collection.immutable._

trait ArTrait[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val descriptor: TraitDescriptor

  val isSealed: Boolean

  val signature: Comp[Signature[ArTrait.ResultInfo]]

  val methods: Comp[Vector[ArMethod[TContext, TPayloadSpec]]]
  val metaType: Comp[MetaClass[ArClass[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Unit, TTraitMetadata]
}

object ArTrait {

  sealed trait ResultInfo[TS <: TypeSystem with Singleton] {
    val typeSystem: TS
    val baseTypes: typeSystem.BaseTypeInfoTrait
  }

  object ResultInfo {
    def apply(ts: TypeSystem)(bt: ts.BaseTypeInfoTrait): ResultInfo[ts.type] = new ResultInfo[ts.type] {
      override val typeSystem: ts.type = ts
      override val baseTypes: typeSystem.BaseTypeInfoTrait = bt
    }
  }

}

trait ArTraitInNamespace[TContext <: Context, TPayloadSpec[_, _]] {
  self: ArTrait[TContext, TPayloadSpec] =>

  override val descriptor: TraitDescriptor.InNamespace
}
