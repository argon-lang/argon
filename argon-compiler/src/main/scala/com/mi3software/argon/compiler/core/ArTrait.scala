package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.types._
import scalaz.Scalaz._
import scalaz._

import scala.collection.immutable._

trait ArTrait[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._, signatureContext.Signature

  val descriptor: TraitDescriptor

  val isSealed: Boolean

  val signature: Comp[Signature[ArTrait.ResultInfo]]

  val methods: Comp[Vector[ArMethod[TContext, TPayloadSpec]]]
  val metaType: Comp[MetaClass[ArClass[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Unit, TTraitMetadata]
}

object ArTrait {

  sealed trait ResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
    val typeSystem: TS
    val baseTypes: typeSystem.BaseTypeInfoTrait
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(bt: ts.BaseTypeInfoTrait): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override val baseTypes: typeSystem.BaseTypeInfoTrait = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[F[_]: Monad]
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverter[context.type, ts1.type, ts2.type, F])
      (result: ResultInfo[context.type, ts1.type])
      : F[ResultInfo[context.type, ts2.type]] =
        for {
          baseTraits <- result.baseTypes.baseTraits.traverse(TypeSystem.convertTraitType(context)(ts1)(ts2)(converter)(_))
        } yield ResultInfo(ts2)(ts2.BaseTypeInfoTrait(baseTraits))
    }
  }

}

trait ArTraitInNamespace[TContext <: Context, TPayloadSpec[_, _]] {
  self: ArTrait[TContext, TPayloadSpec] =>

  override val descriptor: TraitDescriptor.InNamespace
}
