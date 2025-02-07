package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[source] object SourceFunction {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, importFactory: ImportFactory)(decl: ast.FunctionDeclarationStmt): ctx.Comp[ArFuncC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.Implementations.FunctionImplementation]
    yield new ArFuncC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = funcId

      override def isInline: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Inline)
      override def isErased: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Erased)

      override def importSpecifier: Comp[ImportSpecifier] =
        for
          sig <- signature
          erasedSig <- SignatureEraser(ctx).eraseSignature(sig)
        yield importFactory.getImportSpecifier(erasedSig)

      override def signature: Comp[FunctionSignature] = sigCache.get(
        scope.toScope.flatMap { scope =>
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ParameterOwner.Func(this))(decl.parameters, decl.returnType)
        }
      )

      override def implementation: Option[Comp[context.Implementations.FunctionImplementation]] =
        Some(implCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            scope2 = context.Scopes.ParameterScope(context.TRExprContext.ParameterOwner.Func(this), scope, sig.parameters)
            impl <- decl.body.value match {
              case ast.Expr.Extern(name) =>
                ZIO.succeed(ctx.Implementations.FunctionImplementation.Extern(name))


              case _ =>
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

                tr.typeCheckExpr(scope2)(decl.body, sig.returnType)
                  .map(context.Implementations.FunctionImplementation.Expr.apply)
            }
          yield impl
        ))

      override def toString(): String =
        decl.name.toString()

    }
}
