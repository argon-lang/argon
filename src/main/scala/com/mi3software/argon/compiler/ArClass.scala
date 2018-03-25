package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz._
import Scalaz._

sealed trait ArClass[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  type PayloadSpec[_, _]

  val descriptor: ClassDescriptor

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val signature: Signature[typeSystem.type, ArClass.ResultInfo]

  val methods: Comp[Vector[ArMethodWithPayload[TContext, PayloadSpec]]]
  val classConstructors: Comp[Vector[ClassConstructorWithPayload[TContext, PayloadSpec]]]
  val metaType: MetaClass[TContext, ArClassWithPayload[TContext, PayloadSpec]]

  val payload: PayloadSpec[Unit, TClassMetadata]
}

object ArClass {

  final case class ResultInfo[TS <: TypeSystem](baseTypes: BaseTypeInfoClass[TS#TClassInfo, TS#TTraitInfo])


  implicit def equalInstance[TContext <: Context]: Equal[ArClass[TContext]] =
    (a, b) => a.descriptor === b.descriptor

}

trait ArClassWithPayload[TContext <: Context, TPayloadSpec[_, _]] extends ArClass[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}

trait ArClassInNamespace[TContext <: Context] {
  self: ArClass[TContext] =>

  override val descriptor: ClassDescriptor.InNamespace
}
