package dev.argon.compiler.core

import dev.argon.compiler.types._

import scala.collection.immutable._
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.expr.ArExpr.TraitType
import dev.argon.compiler.{Comp, CompilationMessageSource}
import dev.argon.util.FileID
import shapeless.Nat
import zio.IO


trait DataConstructor[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val descriptor: DataConstructorDescriptor
  val fileId: FileID
  val ctorMessageSource: CompilationMessageSource

  val signature: Comp[Signature[DataConstructor.ResultInfo, _ <: Nat]]

  val methods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Comp[TDataConstructorImplementation], TDataConstructorMetadata]
}

object DataConstructor {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    DataConstructor[TContext, TPayloadSpec] { val descriptor: DataConstructorDescriptor.InNamespace }

  final case class ResultInfo[TContext <: Context with Singleton, Wrap[+_]](instanceType: TraitType[TContext, Wrap])

  object ResultInfo {
    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {

      override def convertTypeSystem[Wrap1[+_], Wrap2[+_]]
      (context: Context)
      (converter: TypeSystemConverter.Aux[context.type, Wrap1, Wrap2])
      (result: ResultInfo[context.type, Wrap1])
      : Comp[ResultInfo[context.type, Wrap2]] = for {
        instanceType <- converter.convertTraitType(result.instanceType)
      } yield ResultInfo(instanceType)

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : Comp[Boolean] =
        IO.succeed(refChecker.checkArExpr(result.instanceType))


      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper] =
        ResultInfo(subst.substTraitType(result.instanceType))
    }
  }

}

