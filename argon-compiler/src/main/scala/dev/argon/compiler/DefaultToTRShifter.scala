package dev.argon.compiler

import dev.argon.expr.ContextShifter
import cats.*
import cats.implicits.given

private[compiler] final class DefaultToTRShifter[Ctx <: Context](val context: Ctx) extends ContextShifter[Id] {
  override val ec1: context.DefaultExprContext.type = context.DefaultExprContext
  override val ec2: context.TRExprContext.type = context.TRExprContext

  override protected def shiftHole(hole: ec1.Hole): ec2.Expr = hole
}
