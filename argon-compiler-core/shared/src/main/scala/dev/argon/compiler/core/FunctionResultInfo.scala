package dev.argon.compiler.core

import dev.argon.compiler.types._
import dev.argon.compiler.Comp
import dev.argon.compiler.expr.ArExprWrap
import zio.IO

sealed trait FunctionResultInfo[TContext <: Context with Singleton, Wrap[+_]] {
  val returnType: ArExprWrap[TContext, Wrap]
}

object FunctionResultInfo {

  def apply[TContext <: Context with Singleton, Wrap[+_]](retType: ArExprWrap[TContext, Wrap]): FunctionResultInfo[TContext, Wrap] = new FunctionResultInfo[TContext, Wrap] {
    override val returnType: ArExprWrap[TContext, Wrap] = retType
  }

  implicit val sigResConverterInstance: SignatureResultConverter[FunctionResultInfo] = new SignatureResultConverter[FunctionResultInfo] {
    override def convertTypeSystem[Wrap1[+_], Wrap2[+_]]
    (context: Context)
    (converter: TypeSystemConverter.Aux[context.type, Wrap1, Wrap2])
    (result: FunctionResultInfo[context.type, Wrap1])
    : Comp[FunctionResultInfo[context.type, Wrap2]] = for {
      returnType <- converter.convertTypeSystem(result.returnType)
    } yield FunctionResultInfo(returnType)

    override def referencesParameter
    (signatureContext: SignatureContext)
    (refChecker: signatureContext.RefChecker)
    (result: FunctionResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper]): Comp[Boolean] = {
      IO.succeed(refChecker.checkWrapExpr(result.returnType))
    }

    override def substitute
    (signatureContext: SignatureContext)
    (subst: signatureContext.Subst)
    (result: FunctionResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
    : FunctionResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper] =
      FunctionResultInfo(
        subst.substWrapExpr(result.returnType)
      )
  }

}
