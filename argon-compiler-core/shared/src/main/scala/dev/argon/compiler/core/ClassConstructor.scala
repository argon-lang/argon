package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import dev.argon.compiler.expr.ArExpr.ClassType
import dev.argon.util.FileID
import shapeless.Nat
import zio.IO

trait ClassConstructor[TContext <: Context, TPayloadSpec[_, _]] extends CallableClassConstructor {
  val context: TContext
  import context._, signatureContext.Signature

  val effectInfo: EffectInfo

  val id: ClassConstructorId
  val owner: ClassConstructorOwner[context.type, TPayloadSpec]
  val fileId: FileID

  final def ownerClass: ArClass[context.type, TPayloadSpec] = owner.ownerClass
  val signatureUnsubstituted: Comp[Signature[ClassConstructor.ResultInfo, _ <: Nat]]

  final def signature
  (newSigContext: SignatureContext.Aux[context.type])
  (instanceType: ClassType[context.type, newSigContext.TTypeWrapper])
  : Comp[newSigContext.Signature[ClassConstructor.ResultInfo, _]] =
    for {
      sig <- signatureUnsubstituted
      converter = ArTypeSystemConverter(context)(newSigContext.typeWrapperInstances)
      ownerSigUnConv <- ownerClass.signature
      ownerSig <- ownerSigUnConv.convertTypeSystem(newSigContext)(converter)
      convSig <- sig.convertTypeSystem(newSigContext)(converter)
      newSig = convSig.substituteTypeArguments(ownerSig.unsubstitutedParameters.unsized)(instanceType.args)
    } yield newSig



  val payload: TPayloadSpec[Comp[TClassConstructorImplementation], Unit]
}

object ClassConstructor {

  final case class ResultInfo[TContext <: Context with Singleton, Wrap[+_]]()

  object ResultInfo {

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {

      override def convertTypeSystem[Wrap1[+_], Wrap2[+_]]
      (context: Context)
      (converter: TypeSystemConverter.Aux[context.type, Wrap1, Wrap2])
      (result: ResultInfo[context.type, Wrap1])
      : Comp[ResultInfo[context.type, Wrap2]] =
        IO.succeed(ResultInfo[context.type, Wrap2]())

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : Comp[Boolean] =
        IO.succeed(false)

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper] =
        ResultInfo()
    }

  }

}
