package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.types.TypeSystem

sealed trait ClassConstructor[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._

  val effectInfo: EffectInfo
  val accessModifier: AccessModifier

  val descriptor: ClassConstructorDescriptor

  val signature: Signature[ClassConstructor.ResultInfo]

  val instanceClass: ArClass[TContext, TPayloadSpec]

  val payload: TPayloadSpec[Comp[TClassConstructorImplementation], TClassConstructorMetadata]
}

object ClassConstructor {

  final case class ResultInfo[+TS <: TypeSystem]()

}
