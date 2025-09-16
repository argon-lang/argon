package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[source] object SourceMethod {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, methodOwner: MethodOwner[ctx.type])(decl: ast.MethodDeclarationStmt)(using externProvider: ExternProvider & HasContext[ctx.type]): ctx.Comp[ArMethodC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.MethodImplementation]
      
      erased = decl.modifiers.exists(_.value == ast.Modifier.Erased)
      
      _ <- ErrorLog.logError(CompilerError.ErasedMustBePure(decl.name.location))
        .whenDiscard(erased && !decl.purity)
      
    yield new ArMethodC {
      override val context: ctx.type = ctx
      override val name: IdentifierExpr = decl.name.value
      override val id: UniqueIdentifier = funcId
      override val owner: MethodOwner[context.type] = methodOwner

      override def isInline: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Inline)
      override def isErased: Boolean = erased
      override def isWitness: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Witness)

      override def effects: context.DefaultExprContext.EffectInfo =
        if decl.purity then context.DefaultExprContext.EffectInfo.Pure
        else context.DefaultExprContext.EffectInfo.Effectful

      override def slot: MethodSlot =
        if decl.body.isEmpty then MethodSlot.Abstract
        else MethodSlot.Final

      override def signature: Comp[FunctionSignature] = sigCache.get(
        scope.toScope.flatMap { scope =>
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Method(this))(decl.parameters, decl.returnType)
        }
      )

      override def implementation: Option[Comp[context.implementations.MethodImplementation]] =
        Some(implCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            scope2 = context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Method(this), scope, sig.parameters)
            impl <- decl.body match {
              case None =>
                ZIO.succeed(context.implementations.MethodImplementation.Abstract)

              case Some(ast.FunctionBody.ExternBody(WithLocation(name, loc))) =>
                externProvider.getExternMethod(name)
                  .flatMap {
                    case Some(ext) =>
                      ZIO.succeed(ctx.implementations.MethodImplementation.Extern(ext))

                    case None =>
                      ErrorLog.logError(CompilerError.UnknownExtern(loc, name))
                        .as(context.implementations.MethodImplementation.Expr(
                          context.DefaultExprContext.Expr.Error()
                        ))
                  }


              case Some(ast.FunctionBody.ExprBody(expr)) =>
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

                tr.typeCheckExpr(scope2)(expr, sig.returnType, effects, erased = isErased)
                  .map(context.implementations.MethodImplementation.Expr.apply)
            }
          yield impl
        ))

      override def toString(): String =
        decl.name.toString()

    }
}
