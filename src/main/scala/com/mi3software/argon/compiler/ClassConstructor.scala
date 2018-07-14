package com.mi3software.argon.compiler

sealed trait ClassConstructor[TContext <: Context] {
  val context: TContext
  import context._

  type PayloadSpec[_, _]

  val effectInfo: EffectInfo
  val accessModifier: AccessModifier

  val descriptor: ClassConstructorDescriptor

  val signature: Signature[typeSystem.type, ClassConstructor.ResultInfo]

  val instanceClass: ArClassWithPayload[TContext, PayloadSpec]

  val payload: PayloadSpec[Comp[TClassConstructorImplementation], TClassConstructorMetadata]
}

object ClassConstructor {

  final case class ResultInfo[+TS <: TypeSystem]()

}

trait ClassConstructorWithPayload[TContext <: Context, TPayloadSpec[_, _]] extends ClassConstructor[TContext] {
  override type PayloadSpec[A, B] = TPayloadSpec[A, B]
}
