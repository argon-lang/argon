package dev.argon.compiler.loaders.source

import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr
import dev.argon.compiler.expr.ArExpr._
import shapeless.Id

object ExpressionScopeExtractor {

  def addDeclarationsFrom(context: Context)(stmt: ArExpr[context.type, Id], env: ExpressionConverter.Env[context.type, context.scopeContext.Scope]): ExpressionConverter.Env[context.type, context.scopeContext.Scope] = {
    import context._
    import scopeContext.ScopeExtensions

    stmt match {
      case LetBinding(variable, _, next) =>
        addDeclarationsFrom(context)(next, env.copy(scope = env.scope.addVariable(variable)))

      case Sequence(_, next) =>
        addDeclarationsFrom(context)(next, env)

      case _ => env
    }
  }

  def addDeclarationsCreator(context: Context)(stmt: ArExpr[context.type, Id], env: ExpressionConverter.EnvCreator[context.type]): ExpressionConverter.EnvCreator[context.type] = {

    stmt match {
      case LetBinding(variable, _, next) =>
        addDeclarationsCreator(context)(next, env.addVariable(context)(variable))

      case Sequence(_, next) =>
        addDeclarationsCreator(context)(next, env)

      case _ => env
    }
  }

}
