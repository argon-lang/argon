package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.util.FileID
import shapeless.Nat

trait ArFunc[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._, signatureContext.Signature

  val descriptor: FuncDescriptor
  val fileId: FileID
  val effectInfo: EffectInfo

  val signature: Comp[Signature[FunctionResultInfo, _ <: Nat]]

  val payload: TPayloadSpec[Comp[TFunctionImplementation], TFunctionMetadata]

  override def hashCode(): Int = descriptor.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArFunc[_, _] => other.descriptor === descriptor
    case _ => false
  }
}

object ArFunc {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    ArFunc[TContext, TPayloadSpec] { val descriptor: FuncDescriptor.InNamespace }

}
