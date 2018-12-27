package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.types._
import scala.collection.immutable._
import scalaz._
import Scalaz._

trait ArClass[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val descriptor: ClassDescriptor

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val signature: Comp[Signature[ArClass.ResultInfo]]

  val methods: Comp[Vector[ArMethod[TContext, TPayloadSpec]]]
  val classConstructors: Comp[Vector[ClassConstructor[TContext, TPayloadSpec]]]
  val metaType: Comp[MetaClass[ArClass[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Unit, TClassMetadata]
}

object ArClass {

  sealed trait ResultInfo[TS <: TypeSystem with Singleton] {
    val typeSystem: TS
    val baseTypes: typeSystem.BaseTypeInfoClass
  }

  object ResultInfo {
    def apply(ts: TypeSystem)(bt: ts.BaseTypeInfoClass): ResultInfo[ts.type] = new ResultInfo[ts.type] {
      override val typeSystem: ts.type = ts
      override val baseTypes: typeSystem.BaseTypeInfoClass = bt
    }
  }

}

trait ArClassInNamespace[TContext <: Context, TPayloadSpec[_, _]] {
  self: ArClass[TContext, TPayloadSpec] =>

  override val descriptor: ClassDescriptor.InNamespace
}
