package dev.argon.compiler

import cats.*
import cats.implicits.given
import dev.argon.expr.ContextShifter

private[compiler] final class TRToErrorShifter[Ctx <: Context](val context: Ctx) extends ContextShifter[Id] {
  override val ec1: context.TRExprContext.type = context.TRExprContext
  override val ec2: ErrorExprContext.type = ErrorExprContext

  override protected def shiftHole(hole: ec1.Hole): ec2.Expr = ec2.Expr.Hole(hole)
}
