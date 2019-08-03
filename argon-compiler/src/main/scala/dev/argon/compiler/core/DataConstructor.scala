package dev.argon.compiler.core

import dev.argon.compiler.types._

import scala.collection.immutable._
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.CompilationMessageSource
import dev.argon.util.FileID


trait DataConstructor[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val descriptor: DataConstructorDescriptor
  val fileId: FileID
  val ctorMessageSource: CompilationMessageSource

  val signature: Comp[Signature[DataConstructor.ResultInfo]]

  val methods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Comp[TDataConstructorImplementation], TDataConstructorMetadata]
}

object DataConstructor {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    DataConstructor[TContext, TPayloadSpec] { val descriptor: DataConstructorDescriptor.InNamespace }

  sealed trait ResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
    val typeSystem: TS
    val instanceType: typeSystem.TraitType
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(instance: ts.TraitType): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override val instanceType: typeSystem.TraitType = instance
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[F[_]: Monad]
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverter[context.type, ts1.type, ts2.type, F])
      (result: ResultInfo[context.type, ts1.type])
      : F[ResultInfo[context.type, ts2.type]] = for {
        instanceType <- TypeSystem.convertTraitType(context)(ts1)(ts2)(converter)(result.instanceType)
      } yield ResultInfo(ts2)(instanceType)

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : Boolean =
        refChecker.checkArExpr(result.instanceType)


      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Substitutions)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type] =
        ResultInfo(result.typeSystem)(subst.substTraitType(result.instanceType))
    }
  }

}

