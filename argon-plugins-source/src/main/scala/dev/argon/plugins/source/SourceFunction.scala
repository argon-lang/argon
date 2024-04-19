package dev.argon.plugins.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.plugin.PlatformPluginSet
import dev.argon.util.{*, given}
import zio.*

object SourceFunction {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, externFactory: ExternFactory & HasContext[ctx.type])(decl: ast.FunctionDeclarationStmt): ctx.Comp[ArFuncC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionImplementation]
      refCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionReference]
    yield new ArFuncC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = funcId

      override def isInline: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Inline)
      override def isErased: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Erased)

      override def signature: Comp[FunctionSignature] = sigCache.get(
        scope.toScope.flatMap { scope =>
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ParameterOwner.Func(this))(decl.parameters, decl.returnType)
        }
      )

      override def implementation: Option[Comp[context.implementations.FunctionImplementation]] =
        Some(implCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            scope2 = context.Scopes.ParameterScope(context.TRExprContext.ParameterOwner.Func(this), scope, sig.parameters)
            impl <- decl.body.value match {
              case ast.Expr.Extern(name) =>
                externFactory.getExternFunctionImplementation(name)
                  .flatMap {
                    case Some(e) => ZIO.succeed(ctx.implementations.FunctionImplementation.Extern(e))
                    case None =>
                      for
                        _ <- ErrorLog.logError(CompilerError.UnknownExtern(name))
                      yield context.implementations.FunctionImplementation.Expr(context.DefaultExprContext.Expr.Error())

                  }


              case _ =>
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

                tr.typeCheckExpr(scope2)(decl.body, sig.returnType)
                  .map(context.implementations.FunctionImplementation.Expr.apply)
            }
          yield impl
        ))

      override def reference: Comp[context.implementations.FunctionReference] =
        refCache.get(
          signature
            .flatMap(SignatureEraser(context).eraseSignature)
            .flatMap(externFactory.defineFunctionReference)
        )

      override def toString(): String =
        decl.name.toString()

    }
}
