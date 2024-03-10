package dev.argon.compiler

import dev.argon.expr.ExprContext
import dev.argon.util.UniqueIdentifier

object ErrorExprContext extends ExprContext {
  override type Function = ArFuncC
  override def functionCanEqual: CanEqual[Function, Function] = summon

  override type Hole = UniqueIdentifier
  override def holeCanEqual: CanEqual[Hole, Hole] = summon

}
