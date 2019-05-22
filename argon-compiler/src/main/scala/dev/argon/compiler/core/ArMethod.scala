package dev.argon.compiler.core

import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.util.FileID

abstract class ArMethod[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val descriptor: MethodDescriptor
  val fileId: FileID
  val owner: ArMethod.Owner[context.type, TPayloadSpec]

  val effectInfo: EffectInfo

  val isVirtual: Boolean
  val isAbstract: Boolean
  val isImplicitOverride: Boolean
  val isFinal: Boolean

  val signature: Comp[Signature[FunctionResultInfo]]

  val payload: TPayloadSpec[Comp[TMethodImplementation], TMethodMetadata]

  override def hashCode(): Int = descriptor.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArMethod[_, _] => other.descriptor === descriptor
    case _ => false
  }
}

object ArMethod {

  sealed trait Owner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  final case class ClassOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class ClassObjectOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class TraitOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerTrait: ArTrait[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class TraitObjectOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerTrait: ArTrait[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class DataCtorOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](dataCtor: DataConstructor[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]

}

