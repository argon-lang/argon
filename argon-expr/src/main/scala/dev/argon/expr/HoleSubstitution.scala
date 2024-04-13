package dev.argon.expr

import dev.argon.util.TreeShifter
import cats.*

private trait HoleSubstitution {
  val exprContext: ExprContext
  import exprContext.*

  protected val holeMapping: Map[Hole, Expr]

  private object ShiftHelper extends TreeShifter[Id] {
    import StandardShifters.given

    given exprShifter: Shifter[Expr, Expr] with
      override def shift(a: Expr): Expr =
        (a match {
          case Expr.Hole(h) => holeMapping.get(h)
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

object HoleSubstitution {
  def substitute(ec: ExprContext)(mapping: Map[ec.Hole, ec.Expr])(e: ec.Expr): ec.Expr =
    new HoleSubstitution {
      override val exprContext: ec.type = ec
      override protected val holeMapping: Map[exprContext.Hole, exprContext.Expr] = mapping
    }.substitute(e)
}

