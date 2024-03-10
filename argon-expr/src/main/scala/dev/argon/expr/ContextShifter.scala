package dev.argon.expr

import dev.argon.util.TreeShifter
import cats.*
import cats.implicits.given
import zio.ZIO
import zio.interop.catz.core.given

trait ContextShifter[F[_]: Monad] {

  val ec1: ExprContext
  val ec2: ExprContext {
    type Function >: ec1.Function
  }


  private object ShiftHelper extends TreeShifter[F] {
    import StandardShifters.given

    given exprShifter: Shifter[ec1.Expr, ec2.Expr] = autoShifter
    given Shifter[ec1.Builtin, ec2.Builtin] = autoShifter
    given Shifter[ec1.Expr.Hole, ec2.Expr] with
      override def shift(a: ec1.Expr.Hole): F[ec2.Expr] =
        shiftHole(a.hole)
    end given


    given varShifter: Shifter[ec1.Var, ec2.Var] = autoShifter
    given Shifter[ec1.LocalVar, ec2.LocalVar] = autoShifter

  }

  final def shiftExpr(a: ec1.Expr): F[ec2.Expr] =
    ShiftHelper.exprShifter.shift(a)
    
  final def shiftVar(v: ec1.Var): F[ec2.Var] =
    ShiftHelper.varShifter.shift(v)

  protected def shiftHole(hole: ec1.Hole): F[ec2.Expr]

}
