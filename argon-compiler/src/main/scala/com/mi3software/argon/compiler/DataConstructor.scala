package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz._
import Scalaz._


sealed trait DataConstructor[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  type PayloadSpec[_, _]

  val descriptor: DataConstructorDescriptor

  val signature: Signature[typeSystem.type, DataConstructor.ResultInfo]

  val methods: Comp[Vector[ArMethodWithPayload[TContext, PayloadSpec]]]

  val payload: PayloadSpec[Comp[TDataConstructorImplementation], TDataConstructorMetadata]
}

object DataConstructor {

  final case class ResultInfo[+TS <: TypeSystem](instanceType: TraitType[TS])

  implicit def equalInstance[TContext <: Context]: Equal[DataConstructor[TContext]] =
    (a, b) => a.descriptor === b.descriptor

}

trait DataConstructorWithPayload[TContext <: Context, TPayloadSpec[_, _]] extends DataConstructor[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}

