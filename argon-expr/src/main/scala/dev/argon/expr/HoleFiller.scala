package dev.argon.expr

import cats.*

trait HoleFiller[EC <: ExprContext] extends ContextShifter[Id] {
  override val ec1: EC
  override val ec2: ec1.type

  import ec1.*

  val model: Model

  override protected def shiftHole(hole: Hole): Expr =
    model.resolveHole(hole) match {
      case Some(e) => shiftExpr(e)
      case None => Expr.Hole(hole)
    }
}
