package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.util.{*, given}

object ExprToHolesConverter {
  def apply
  (ctx: Context)
  (holesExprContext: ArgonExprContext & HasContext[ctx.type] { type THole = UniqueIdentifier })
  : ExprProcessor[Id] & HasContext[ctx.type] { val ec1: ctx.ExprContext.type; val ec2: holesExprContext.type } =
    new ExprProcessor[Id]:
      override val context: ctx.type = ctx
      override val ec1: ctx.ExprContext.type = ctx.ExprContext
      override val ec2: holesExprContext.type = holesExprContext

      override def processHole(hole: Nothing): Id[ec2.WrapExpr] = hole
    end new
}
