package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import dev.argon.util.FileID
import cats._
import cats.evidence.Is
import cats.implicits._
import shapeless.Nat
import zio.IO
import zio.interop.catz._

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

  sealed trait ResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
    val typeSystem: TS
    def baseTypes: Comp[typeSystem.BaseTypeInfoTrait]
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(bt: => Comp[ts.BaseTypeInfoTrait]): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override lazy val baseTypes: Comp[typeSystem.BaseTypeInfoTrait] = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverter.Aux[context.type, ts1.type, ts2.type])
      (result: ResultInfo[context.type, ts1.type])
      : Comp[ResultInfo[context.type, ts2.type]] = {
        IO.succeed(ResultInfo(ts2)(
          for {
            baseTypes <- result.baseTypes
            baseTraits <- baseTypes.baseTraits.traverse(converter.convertTraitType(_))
          } yield ts2.BaseTypeInfoTrait(baseTraits)
        ))

      }

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : Comp[Boolean] =
        result.baseTypes.map { baseTypes =>
          baseTypes.baseTraits.exists(refChecker.checkArExpr)
        }

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type] =
        ResultInfo(signatureContext.typeSystem)(
          result.baseTypes.map { baseTypes =>
            signatureContext.typeSystem.BaseTypeInfoTrait(
              baseTypes.baseTraits.map(subst.substTraitType(_))
            )
          }
        )
    }
  }

}
