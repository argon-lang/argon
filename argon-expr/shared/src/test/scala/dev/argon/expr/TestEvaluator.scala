package dev.argon.expr

import zio.{ZIO, IO}

class TestEvaluator[R, E] extends Evaluator[R, E] {
  override val exprContext: TestExprContext.type = TestExprContext
  import exprContext._

  override def getFunctionBody(function: TFunction, args: Vector[WrapExpr], fuel: Int): ZIO[R, E, Option[WrapExpr]] =
    IO.none

  override def getMethodBody(method: TMethod, instance: WrapExpr, args: Vector[WrapExpr], fuel: Int)
    : ZIO[R, E, Option[WrapExpr]] = IO.none

}
