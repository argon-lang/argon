package dev.argon.compiler.core

import dev.argon.compiler.types._
import cats._
import cats.implicits._

sealed trait FunctionResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
  val typeSystem: TS
  val returnType: typeSystem.TType
}

object FunctionResultInfo {

  def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(retType: ts.TType): FunctionResultInfo[TContext, ts.type] = new FunctionResultInfo[TContext, ts.type] {
    override val typeSystem: ts.type = ts
    override val returnType: typeSystem.TType = retType
  }

  implicit val sigResConverterInstance: SignatureResultConverter[FunctionResultInfo] = new SignatureResultConverter[FunctionResultInfo] {
    override def convertTypeSystem[F[_]: Monad]
    (context: Context)
    (ts1: TypeSystem[context.type])
    (ts2: TypeSystem[context.type])
    (converter: TypeSystemConverterEffect.Aux[context.type, ts1.type, ts2.type, F])
    (result: FunctionResultInfo[context.type, ts1.type])
    : F[FunctionResultInfo[context.type, ts2.type]] = for {
      returnType <- converter.convertTypeSystem(result.returnType)
    } yield FunctionResultInfo(ts2)(returnType)

    override def referencesParameter
    (signatureContext: SignatureContext)
    (refChecker: signatureContext.RefChecker)
    (result: FunctionResultInfo[signatureContext.context.type, signatureContext.typeSystem.type]): signatureContext.typeSystem.TSComp[Boolean] = {
      import signatureContext.typeSystem.{ TSComp, tscompCompilationInstance }
      refChecker.checkWrapExpr(result.returnType).pure[TSComp]
    }

    override def substitute
    (signatureContext: SignatureContext)
    (subst: signatureContext.Subst)
    (result: FunctionResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
    : FunctionResultInfo[signatureContext.context.type, signatureContext.typeSystem.type] =
      FunctionResultInfo(signatureContext.typeSystem)(
        subst.substWrapExpr(result.returnType)
      )
  }

}
