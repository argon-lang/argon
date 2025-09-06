package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[source] object SourceFunction {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, importFactory: ImportFactory)(decl: ast.FunctionDeclarationStmt)(using externProvider: ExternProvider & HasContext[ctx.type]): ctx.Comp[ArFuncC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionImplementation]
      
      erased = decl.modifiers.exists(_.value == ast.Modifier.Erased)
      
      _ <- ErrorLog.logError(CompilerError.ErasedMustBePure(decl.name.location))
        .whenDiscard(erased && !decl.purity)
      
    yield new ArFuncC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = funcId

      override def isInline: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Inline)
      override def isErased: Boolean = erased
      override def isWitness: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Witness)

      override def effects: context.DefaultExprContext.EffectInfo =
        if decl.purity then context.DefaultExprContext.EffectInfo.Pure
        else context.DefaultExprContext.EffectInfo.Effectful

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

      override def implementation: Option[Comp[context.implementations.FunctionImplementation]] =
        Some(implCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            scope2 = context.Scopes.ParameterScope(context.TRExprContext.ParameterOwner.Func(this), scope, sig.parameters)
            impl <- decl.body match {
              case ast.FunctionBody.ExternBody(WithLocation(name, loc)) =>
                externProvider.getExternFunction(name)
                  .flatMap {
                    case Some(ext) =>
                      ZIO.succeed(ctx.implementations.FunctionImplementation.Extern(ext))

                    case None =>
                      ErrorLog.logError(CompilerError.UnknownExtern(loc, name))
                        .as(context.implementations.FunctionImplementation.Expr(
                          context.DefaultExprContext.Expr.Error()
                        ))
                  }


              case ast.FunctionBody.ExprBody(expr) =>
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

                tr.typeCheckExpr(scope2)(expr, sig.returnType, effects, erased = isErased)
                  .map(context.implementations.FunctionImplementation.Expr.apply)
            }
          yield impl
        ))

      override def toString(): String =
        decl.name.toString()

    }
}
