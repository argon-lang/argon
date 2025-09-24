package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import cats.*
import dev.argon.ast.IdentifierExpr

private abstract class Substitution[EC <: ExprContext](val exprContext: EC) extends ExprShifter[Id] {
  import exprContext.*

  override final val ec1: exprContext.type = exprContext
  override final val ec2: exprContext.type = exprContext

  protected val variableMapping: Map[Var, Expr]

  override protected def exprShifter: Shifter[ec1.Expr, ec2.Expr] = a =>
    (a match {
      case Expr.Variable(v) =>
        variableMapping.get(v)
      case _ => None
    }).getOrElse(super.exprShifter.shift(a))

  override protected def shiftHole(hole: Hole): Expr =
    Expr.Hole(hole)

  final def substitute(e: Expr): Expr =
    exprShifter.shift(e)

}

object Substitution {
  def substitute(ec: ExprContext)(mapping: Map[ec.Var, ec.Expr])(e: ec.Expr): ec.Expr =
    new Substitution[ec.type](ec) {
      override protected val variableMapping: Map[exprContext.Var, exprContext.Expr] = mapping
    }.substitute(e)
}

