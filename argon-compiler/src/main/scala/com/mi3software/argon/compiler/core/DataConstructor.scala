package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.types._
import scala.collection.immutable._
import scalaz._
import Scalaz._


sealed trait DataConstructor[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._, signatureContext.Signature

  val descriptor: DataConstructorDescriptor

  val signature: Comp[Signature[DataConstructor.ResultInfo]]

  val methods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Comp[TDataConstructorImplementation], TDataConstructorMetadata]
}

object DataConstructor {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    DataConstructor[TContext, TPayloadSpec] { val descriptor: DataConstructorDescriptor.InNamespace }

  sealed trait ResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
    val typeSystem: TS
    val instanceType: typeSystem.TraitType
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(instance: ts.TraitType): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override val instanceType: typeSystem.TraitType = instance
    }
  }

}

