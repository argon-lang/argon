package dev.argon.expr

import dev.argon.util.TreeShifter
import cats.*
import cats.implicits.given
import zio.ZIO
import zio.interop.catz.core.given

trait ContextShifter[F[_]: Monad] {

  val ec1: ExprContext
  val ec2: ExprContext {
    type Function = ec1.Function
  }


  private object ShiftHelper extends TreeShifter[F] {
    import StandardShifters.given

    given Shifter[ec1.WExpr, ec2.WExpr] = shiftExpr

    given exprShifter: Shifter[ec1.Expr, ec2.Expr] = autoShifter
    given Shifter[ec1.Builtin, ec2.Builtin] = autoShifter

    given Shifter[ec1.Var, ec2.Var] = shiftVar
    given Shifter[ec1.LocalVar, ec2.LocalVar] = shiftLocalVar

  }

  def shiftExpr(a: ec1.WExpr): F[ec2.WExpr] =
    a match
      case ec1.WExpr.Normal(e) => ShiftHelper.exprShifter.shift(e).map(ec2.WExpr.Normal.apply)
      case ec1.WExpr.Hole(h) => shiftHole(h)
      case ec1.WExpr.Error() => Monad[F].pure(ec2.WExpr.Error())
    end match

  protected def shiftVar(v: ec1.Var): F[ec2.Var]
  protected def shiftLocalVar(v: ec1.LocalVar): F[ec2.LocalVar]
  protected def shiftHole(hole: ec1.Hole): F[ec2.WExpr]

}
