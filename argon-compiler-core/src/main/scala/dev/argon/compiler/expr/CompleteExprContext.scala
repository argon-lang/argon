package dev.argon.compiler.expr

abstract class CompleteExprContext extends ArgonExprContext {
  override type THole = Nothing
  override given holeCanEqual: CanEqual[THole, THole] = CanEqual.canEqualAny
}
