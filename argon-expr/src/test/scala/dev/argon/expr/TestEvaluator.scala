package dev.argon.expr

import zio.{ZIO, IO}

class TestEvaluator[R, E] extends Evaluator[R, E] {
  override val exprContext: TestExprContext.type = TestExprContext
  import exprContext.*

  override def getFunctionBody(function: TFunction, args: Seq[WrapExpr], fuel: Int): ZIO[R, E, Option[WrapExpr]] =
    ZIO.none

  override def getMethodBody(method: TMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int)
    : ZIO[R, E, Option[WrapExpr]] = ZIO.none

  override def substituteVariables(varMap: Map[String, WrapExpr])(expr: WrapExpr): WrapExpr =
    expr
}
