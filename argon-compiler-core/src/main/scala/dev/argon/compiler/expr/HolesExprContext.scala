package dev.argon.compiler.expr

import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.util.UniqueIdentifier

abstract class HolesExprContext extends ArgonExprContext {
  override type THole = UniqueIdentifier
  override def holeCanEqual: CanEqual[THole, THole] = summon[CanEqual[UniqueIdentifier, UniqueIdentifier]]
}
