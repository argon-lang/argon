package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import dev.argon.ast.IdentifierExpr
import cats.*

private trait HoleSubstitution[EC <: ExprContext](val exprContext: EC) extends ExprShifter[Id] {
  import exprContext.*

  protected val holeMapping: Map[Hole, Expr]

  override final val ec1: exprContext.type = exprContext
  override final val ec2: exprContext.type = exprContext

  override protected def exprShifter: Shifter[ec1.Expr, ec2.Expr] = a =>
    (a match {
      case Expr.Hole(h) => holeMapping.get(h)
      case _ => None
    }).getOrElse(super.exprShifter.shift(a))

  override protected def shiftHole(hole: Hole): Expr =
    Expr.Hole(hole)

  final def substitute(e: Expr): Expr =
    exprShifter.shift(e)

}

object HoleSubstitution {
  def substitute(ec: ExprContext)(mapping: Map[ec.Hole, ec.Expr])(e: ec.Expr): ec.Expr =
    new HoleSubstitution[ec.type](ec) {
      override protected val holeMapping: Map[exprContext.Hole, exprContext.Expr] = mapping
    }.substitute(e)
}

