package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import cats._
import cats.implicits._
import dev.argon.util.FileID

trait ClassConstructor[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._, signatureContext.Signature

  val effectInfo: EffectInfo

  val descriptor: ClassConstructorDescriptor
  val fileId: FileID

  val ownerClass: ArClass[context.type, TPayloadSpec]
  val signatureUnsubstituted: context.Comp[Signature[ClassConstructor.ResultInfo]]

  final def signature[TComp[_]: Compilation]
  (newSigContext: SignatureContext.Aux[context.type])
  (instanceType: newSigContext.typeSystem.ClassType)
  : Comp[newSigContext.Signature[ClassConstructor.ResultInfo]] = for {
    sig <- signatureUnsubstituted
    ownerSigUnConv <- ownerClass.signature
  } yield {
    val converter = ArTypeSystemConverter(context)(newSigContext.typeSystem)
    val ownerSig = ownerSigUnConv.convertTypeSystem(newSigContext)(converter)
    val convSig = sig.convertTypeSystem(newSigContext)(converter)

    convSig.substituteTypeArguments(ownerSig.unsubstitutedParameters)(instanceType.args)
  }


  val payload: TPayloadSpec[Comp[TClassConstructorImplementation], TClassConstructorMetadata]
}

object ClassConstructor {

  final case class ResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton]()

  object ResultInfo {

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[F[_]: Monad]
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverter[context.type, ts1.type, ts2.type, F])
      (result: ResultInfo[context.type, ts1.type])
      : F[ResultInfo[context.type, ts2.type]] =
        ResultInfo[context.type, ts2.type]().pure[F]

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : Boolean =
        false

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type] =
        ResultInfo()
    }

  }

}
