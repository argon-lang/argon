package dev.argon.compiler.core

import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.{Comp, Compilation}
import dev.argon.compiler.types._
import dev.argon.util.FileID
import shapeless.Nat

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

  val signatureUnsubstituted: Comp[Signature[FunctionResultInfo, _ <: Nat]]

  final def signature
  (newSigContext: SignatureContext.Aux[context.type])
  (instanceType: TypeWithMethods[context.type, newSigContext.TTypeWrapper])
  : Comp[newSigContext.Signature[FunctionResultInfo, _]] = {
    for {
      sig <- signatureUnsubstituted
      converter = ArTypeSystemConverter[newSigContext.TTypeWrapper](context)(newSigContext.typeWrapperInstances)
      ownerSig <- owner match {
        case ArMethod.ClassOwner(ownerClass) => ownerClass.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }
        case ArMethod.ClassObjectOwner(ownerClass) => ownerClass.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }
        case ArMethod.TraitOwner(ownerTrait) => ownerTrait.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }
        case ArMethod.TraitObjectOwner(ownerTrait) => ownerTrait.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }
        case ArMethod.DataCtorOwner(dataCtor) => dataCtor.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }
      }

      convSig <- sig.convertTypeSystem(newSigContext)(converter)

      instTypeArgs = instanceType match {
        case TraitType(_, args) => args
        case ClassType(_, args) => args
        case DataConstructorType(_, args, _) => args
      }

      newSig = convSig.substituteTypeArguments(ownerSig.unsubstitutedParameters)(instTypeArgs)

    } yield newSig
  }

  val payload: TPayloadSpec[Comp[TMethodImplementation], TMethodMetadata]

  override def hashCode(): Int = descriptor.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArMethod[_, _] => other.descriptor === descriptor
    case _ => false
  }

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String =
    descriptor.toString
}

object ArMethod {

  sealed trait Owner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  final case class ClassOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class ClassObjectOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class TraitOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerTrait: ArTrait[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class TraitObjectOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerTrait: ArTrait[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]
  final case class DataCtorOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](dataCtor: DataConstructor[TContext, TPayloadSpec]) extends Owner[TContext, TPayloadSpec]

}

