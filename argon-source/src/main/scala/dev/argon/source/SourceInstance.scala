package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[source] object SourceInstance {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, importFactory: ImportFactory)(decl: ast.InstanceDeclarationStmt)(using externProvider: ExternProvider & HasContext[ctx.type]): ctx.Comp[ArInstanceC & HasContext[ctx.type]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      methodsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[ArMethodC & HasContext[ctx.type]]]
    yield new ArInstanceC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = recId

      override def importSpecifier: Comp[ImportSpecifier] =
        for
          sig <- signature
          erasedSig <- SignatureEraser(ctx).eraseSignature(sig)
        yield importFactory.getImportSpecifier(erasedSig)

      override def signature: Comp[FunctionSignature] = sigCache.get(
        scope.toScope.flatMap { scope =>
          val rt = SourceSignature.getTypeSigReturnType(decl.name, decl.returnType)
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Instance(this))(decl.parameters, rt)
        }
      )

      override def methods: Comp[Seq[ArMethod]] =
        methodsCache.get(
          scope.toScope
            .flatMap { scope =>
              signature.map { sig =>
                context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Instance(this), scope, sig.parameters)
              }
            }
            .flatMap { scope2 =>
              ZIO.foreach(decl.body.collect { case WithLocation(method: ast.MethodDeclarationStmt, loc) => method })(
                SourceMethod.make(ctx)(scope2, MethodOwner.ByInstance(this))
              )
            }
        )

      override def toString(): String =
        decl.name.toString()
    }
}
