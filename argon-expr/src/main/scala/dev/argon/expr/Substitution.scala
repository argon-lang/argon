package dev.argon.expr

import dev.argon.util.TreeShifter
import cats.*

private trait Substitution {
  val exprContext: ExprContext
  import exprContext.*

  protected val variableMapping: Map[Var, Expr]

  private object ShiftHelper extends TreeShifter[Id] {
    import StandardShifters.given

    given exprShifter: Shifter[Expr, Expr] with
      override def shift(a: Expr): Expr =
        (a match {
          case Expr.Variable(v) => variableMapping.get(v)
          case _ => None
        }).getOrElse(autoShifter[Expr, Expr].shift(a))
    end exprShifter

    given Shifter[Builtin, Builtin] = autoShifter
    given Shifter[Hole, Hole] = identity


    given Shifter[Var, Var] = autoShifter
    given Shifter[LocalVar, LocalVar] = autoShifter

  }

  final def substitute(e: Expr): Expr =
    ShiftHelper.exprShifter.shift(e)

}

object Substitution {
  def substitute(ec: ExprContext)(mapping: Map[ec.Var, ec.Expr])(e: ec.Expr): ec.Expr =
    new Substitution {
      override val exprContext: ec.type = ec
      override protected val variableMapping: Map[exprContext.Var, exprContext.Expr] = mapping
    }.substitute(e)
}

