package dev.argon.compiler.loaders.source

import dev.argon.compiler.core._

object ExpressionScopeExtractor {

  def addDeclarationsFrom(context: Context)(stmt: context.typeSystem.ArExpr, env: ExpressionConverter.Env[context.type, context.scopeContext.Scope]): ExpressionConverter.Env[context.type, context.scopeContext.Scope] = {
    import context._
    import scopeContext.ScopeExtensions

    stmt match {
      case typeSystem.LetBinding(variable, _, next) =>
        addDeclarationsFrom(context)(next, env.copy(scope = env.scope.addVariable(variable)))

      case typeSystem.Sequence(_, next) =>
        addDeclarationsFrom(context)(next, env)

      case _ => env
    }
  }

  def addDeclarationsCreator(context: Context)(stmt: context.typeSystem.ArExpr, env: ExpressionConverter.EnvCreator[context.type]): ExpressionConverter.EnvCreator[context.type] = {
    import context._

    stmt match {
      case typeSystem.LetBinding(variable, _, next) =>
        addDeclarationsCreator(context)(next, env.addVariable(context)(variable))

      case typeSystem.Sequence(_, next) =>
        addDeclarationsCreator(context)(next, env)

      case _ => env
    }
  }

}
