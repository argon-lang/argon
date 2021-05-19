package dev.argon.compiler.core

import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.Comp
import dev.argon.compiler.types._
import dev.argon.util.FileID
import shapeless.Nat

abstract class ArMethod[TContext <: Context with Singleton, TPayloadSpec[_, _]] extends CallableMethod {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val id: MethodId
  val owner: MethodOwner[context.type, TPayloadSpec]
  val name: MethodName

  val fileId: FileID

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
    val converter = ArTypeSystemConverter[newSigContext.TTypeWrapper](context)(newSigContext.typeWrapperInstances)
    owner match {
      case MethodOwner.ByClass(ownerClass) => ownerClass.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }.flatMap(signatureByOwnerSig(newSigContext)(instanceType)(_))
      case MethodOwner.ByClassObject(ownerClass) => ownerClass.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }.flatMap(signatureByOwnerSig(newSigContext)(instanceType)(_))
      case MethodOwner.ByTrait(ownerTrait) => ownerTrait.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }.flatMap(signatureByOwnerSig(newSigContext)(instanceType)(_))
      case MethodOwner.ByTraitObject(ownerTrait) => ownerTrait.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }.flatMap(signatureByOwnerSig(newSigContext)(instanceType)(_))
      case MethodOwner.ByDataCtor(dataCtor) => dataCtor.signature.flatMap { _.convertTypeSystem(newSigContext)(converter) }.flatMap(signatureByOwnerSig(newSigContext)(instanceType)(_))
    }
  }

  private def signatureByOwnerSig[TResult[TContext2 <: Context with Singleton, Wrap[+_]]]
  (newSigContext: SignatureContext.Aux[context.type])
  (instanceType: TypeWithMethods[context.type, newSigContext.TTypeWrapper])
  (ownerSig: newSigContext.Signature[TResult, _ <: Nat])
  : Comp[newSigContext.Signature[FunctionResultInfo, _]] = for {
    sig <- signatureUnsubstituted
    converter = ArTypeSystemConverter[newSigContext.TTypeWrapper](context)(newSigContext.typeWrapperInstances)
    convSig <- sig.convertTypeSystem(newSigContext)(converter)
    instTypeArgs = instanceType match {
      case TraitType(_, args) => args
      case ClassType(_, args) => args
      case DataConstructorType(_, args, _) => args
    }
  } yield convSig.substituteTypeArguments(ownerSig.unsubstitutedParameters.unsized)(instTypeArgs)

  val payload: TPayloadSpec[Comp[TMethodImplementation], Unit]

  override def hashCode(): Int = id.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArMethod[_, _] => other.id === id
    case _ => false
  }
}

