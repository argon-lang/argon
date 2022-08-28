package dev.argon.compiler.expr

import dev.argon.expr.ExprConstraints

trait ExprUtilOptions
  extends ExprUtilBase
    with ExprUtilAccess
    with ExprUtilScope
    with ExprUtilImplicitSource
 {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr, FunctionResultVariable}

  final case class Env
  (
    scope: Scope,
    model: Map[exprContext.THole, ExprConstraints[WrapExpr]],
    knownVarValues: Map[Variable, WrapExpr],
    implicitSource: ImplicitSource,
  ) {
    def withScope(f: Scope => Scope): Env = copy(scope = f(scope))
    def withImplicitSource(f: ImplicitSource => ImplicitSource): Env = copy(implicitSource = f(implicitSource))

    def mergeBranches(first: Env, second: Env): Env = this
  }

  final case class ExprOptions
  (
    purity: Boolean,
    accessToken: AccessToken,
    allowAbstractConstructorCall: Boolean,
    allowErased: Boolean,
    postconditions: Option[Postconditions],
  ) {
    def forTypeExpr: ExprOptions =
      requirePure.allowErased(true)

    def requirePure: ExprOptions =
      requirePurity(true)

    def requirePurity(purity: Boolean): ExprOptions =
      copy(purity = this.purity && purity)

    def checkPurity(callablePurity: Boolean): Boolean =
      callablePurity || !purity

    def allowErased(allow: Boolean): ExprOptions =
      copy(allowErased = allowErased || allow)

    def checkErasure(erased: Boolean): Boolean =
      allowErased || !erased
  }

  final case class Postconditions
  (
    resultVar: FunctionResultVariable,
    conditions: Seq[WrapExpr],
  )

}
