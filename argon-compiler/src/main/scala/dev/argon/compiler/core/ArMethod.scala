package dev.argon.compiler.core

import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.Compilation
import dev.argon.compiler.types.{ArTypeSystemConverter, TypeSystem, TypeSystemConverter}
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

  val signatureUnsubstituted: Comp[Signature[FunctionResultInfo]]

  def signature[TComp[_]: Compilation]
  (newSigContext: SignatureContext.Aux[context.type])
  (instanceType: newSigContext.typeSystem.TypeWithMethods)
  : Comp[newSigContext.Signature[FunctionResultInfo]] = for {
    sig <- signatureUnsubstituted
    converter = ArTypeSystemConverter(context)(newSigContext.typeSystem)
    ownerSig <- owner match {
      case ArMethod.ClassOwner(ownerClass) => ownerClass.signature.map { _.convertTypeSystem(newSigContext)(converter) }
      case ArMethod.ClassObjectOwner(ownerClass) => ownerClass.signature.map { _.convertTypeSystem(newSigContext)(converter) }
      case ArMethod.TraitOwner(ownerTrait) => ownerTrait.signature.map { _.convertTypeSystem(newSigContext)(converter) }
      case ArMethod.TraitObjectOwner(ownerTrait) => ownerTrait.signature.map { _.convertTypeSystem(newSigContext)(converter) }
      case ArMethod.DataCtorOwner(dataCtor) => dataCtor.signature.map { _.convertTypeSystem(newSigContext)(converter) }
    }
  } yield {

    val instTypeArgs = instanceType match {
      case newSigContext.typeSystem.TraitType(_, args, _) => args
      case newSigContext.typeSystem.ClassType(_, args, _) => args
      case newSigContext.typeSystem.DataConstructorType(_, args, _) => args
    }
    val convSig = sig.convertTypeSystem(newSigContext)(converter)

    def handleNonReplacableParam(sig: newSigContext.Signature[FunctionResultInfo])(param: newSigContext.typeSystem.Parameter): newSigContext.Signature[FunctionResultInfo] =
      if(!sig.referencesParameter(param))
        sig
      else
        ???

    ownerSig.unsubstitutedParameters.zip(instTypeArgs).foldLeft(convSig) {
      case (sig, (param, newSigContext.typeSystem.TypeArgument.Expr(arg))) =>
        newSigContext.typeSystem.unwrapType(arg) match {
          case Some(arg) => sig.substitute(param)(arg)
          case None => handleNonReplacableParam(sig)(param)
        }

      case (sig, (param, newSigContext.typeSystem.TypeArgument.Wildcard)) =>
        handleNonReplacableParam(sig)(param)
    }
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

