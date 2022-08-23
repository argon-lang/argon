package dev.argon.compiler.expr

import dev.argon.expr.ExprConstraints

trait ExprUtilOptions
  extends ExprUtilBase
    with ExprUtilAccess
    with ExprUtilScope
 {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  final case class Env
  (
    scope: Scope,
    model: Map[exprContext.THole, ExprConstraints[WrapExpr]],
  ) {
    def withScope(f: Scope => Scope): Env = copy(scope = f(scope))

    def mergeBranches(first: Env, second: Env): Env = this

    def allowErased(allow: Boolean): Env = this
  }

  final case class ExprOptions
  (
    purity: Boolean,
    accessToken: AccessToken,
    allowAbstractConstructorCall: Boolean,
  ) {
    def forTypeExpr: ExprOptions =
      requirePure

    def requirePure: ExprOptions =
      requirePurity(true)

    def requirePurity(purity: Boolean): ExprOptions =
      copy(purity = this.purity && purity)

    def checkPurity(callablePurity: Boolean): Boolean =
      callablePurity || !purity
  }

}
