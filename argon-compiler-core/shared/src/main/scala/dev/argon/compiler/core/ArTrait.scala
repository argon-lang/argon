package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import dev.argon.util.FileID
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.expr.BaseTypeInfoTrait
import shapeless.Nat
import zio.{IO, ZIO}

import scala.collection.immutable._

trait ArTrait[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val descriptor: TraitDescriptor
  val fileId: FileID

  val isSealed: Boolean

  val signature: Comp[Signature[ArTrait.ResultInfo, _ <: Nat]]

  val methods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]
  val staticMethods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Unit, TTraitMetadata]

  override def hashCode(): Int = descriptor.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArTrait[_, _] => other.descriptor === descriptor
    case _ => false
  }
}

object ArTrait {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    ArTrait[TContext, TPayloadSpec] { val descriptor: TraitDescriptor.InNamespace }

  sealed trait ResultInfo[TContext <: Context with Singleton, Wrap[+_]] {
    def baseTypes: Comp[BaseTypeInfoTrait[TContext, Wrap]]
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton, Wrap[+_]](bt: => Comp[BaseTypeInfoTrait[TContext, Wrap]]): ResultInfo[TContext, Wrap] = new ResultInfo[TContext, Wrap] {
      override lazy val baseTypes: Comp[BaseTypeInfoTrait[TContext, Wrap]] = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[Wrap1[+_], Wrap2[+_]]
      (context: Context)
      (converter: TypeSystemConverter.Aux[context.type, Wrap1, Wrap2])
      (result: ResultInfo[context.type, Wrap1])
      : Comp[ResultInfo[context.type, Wrap2]] =
        IO.succeed(ResultInfo(
          for {
            baseTypes <- result.baseTypes
            baseTraits <- ZIO.foreach(baseTypes.baseTraits)(converter.convertTraitType(_))
          } yield BaseTypeInfoTrait(baseTraits.toVector)
        ))

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : Comp[Boolean] =
        result.baseTypes.map { baseTypes =>
          baseTypes.baseTraits.exists(refChecker.checkArExpr)
        }

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper] =
        ResultInfo(
          result.baseTypes.map { baseTypes =>
            BaseTypeInfoTrait(
              baseTypes.baseTraits.map(subst.substTraitType(_))
            )
          }
        )
    }
  }

}
