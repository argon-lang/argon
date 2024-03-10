package dev.argon.plugins.source

import dev.argon.ast
import dev.argon.compiler.{ArFuncC, Context, HasContext, SignatureEraser, TypeResolver}
import dev.argon.plugin.PlatformPluginSet
import dev.argon.util.*

object SourceFunction {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, refFactory: ReferenceFactory & HasContext[ctx.type])(decl: ast.FunctionDeclarationStmt): ctx.Comp[ArFuncC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionImplementation]
      refCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionReference]
    yield new ArFuncC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = funcId

      override def signature: Comp[FunctionSignature] = sigCache.get(
        scope.toScope.flatMap { scope =>
          SourceSignature.parse(ctx)(scope)(this)(decl.parameters, decl.returnType)
        }
      )

      override def implementation: Option[Comp[context.implementations.FunctionImplementation]] =
        Some(implCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            scope2 = context.Scopes.ParameterScope(this, scope, sig.parameters)
            impl <- decl.body.value match {
              case ast.Expr.Extern(name) => ???
              case _ =>
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

                tr.typeCheckExpr(scope2)(decl.body, sig.returnType)
                  .map(ctx.implementations.FunctionImplementation.Expr.apply)
            }
          yield impl
        ))

      override def reference: Comp[context.implementations.FunctionReference] =
        refCache.get(
          signature
            .flatMap(SignatureEraser(context).eraseSignature)
            .flatMap(refFactory.defineFunctionReference)
        )

    }
}
