package dev.argon.compiler.source

import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.util.UniqueIdentifier

abstract class SourceCompilerExprContext extends ArgonExprContext {
  override type THole = UniqueIdentifier
  override def holeCanEqual: CanEqual[THole, THole] = summon[CanEqual[UniqueIdentifier, UniqueIdentifier]]
}
