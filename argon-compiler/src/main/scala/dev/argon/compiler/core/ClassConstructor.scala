package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import scalaz._
import Scalaz._

trait ClassConstructor[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._, signatureContext.Signature

  val effectInfo: EffectInfo

  val descriptor: ClassConstructorDescriptor

  val ownerClass: ArClass[context.type, TPayloadSpec]
  val signature: context.Comp[Signature[ClassConstructor.ResultInfo]]

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
        ResultInfo[context.type, ts2.type]().point[F]
    }
  }

}
