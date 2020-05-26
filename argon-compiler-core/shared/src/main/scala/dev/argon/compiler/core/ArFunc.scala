package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.compiler.Comp
import dev.argon.util.FileID
import shapeless.Nat

abstract class ArFunc[TContext <: Context, TPayloadSpec[_, _]] extends CallableFunction {
  val context: TContext
  import context._, signatureContext.Signature

  val id: FunctionId
  val owner: FunctionOwner[context.type, TPayloadSpec]
  val fileId: FileID
  val effectInfo: EffectInfo

  val signature: Comp[Signature[FunctionResultInfo, _ <: Nat]]

  val payload: TPayloadSpec[Comp[TFunctionImplementation], TFunctionMetadata]

  override def hashCode(): Int = id.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArFunc[_, _] => other.id === id
    case _ => false
  }
}

object ArFunc {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    ArFunc[TContext, TPayloadSpec] { val owner: FunctionOwner.ByNamespace[TContext, TPayloadSpec] }

}
