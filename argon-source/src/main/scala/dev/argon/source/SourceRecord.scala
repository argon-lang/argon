package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*
import dev.argon.ast.IdentifierExpr

private[source] object SourceRecord {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, importFactory: ImportFactory)(decl: ast.RecordDeclarationStmt): ctx.Comp[ArRecordC & HasContext[ctx.type]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      fieldsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[RecordFieldC & HasContext[ctx.type]]]
    yield new ArRecordC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = recId

      override def importSpecifier: Comp[ImportSpecifier] =
        for
          sig <- signature
          erasedSig <- SignatureEraser(ctx).eraseSignature(sig)
        yield importFactory.getImportSpecifier(erasedSig)

      override def signature: Comp[FunctionSignature] = sigCache.get(
        scope.toScope.flatMap { scope =>
          val rt = decl.returnType match {
            case Some(rt) =>
              WithLocation(
                ast.ReturnTypeSpecifier(
                  returnType = rt,
                  ensuresClauses = Seq(),
                ),
                rt.location,
              )

            case None =>
              WithLocation(
                ast.ReturnTypeSpecifier(
                  returnType = WithLocation(
                    ast.Expr.Type(None),
                    decl.name.location,
                  ),
                  ensuresClauses = Seq(),
                ),
                decl.name.location,
              )
          }

          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ParameterOwner.Rec(this))(decl.parameters, rt)
        }
      )

      override def fields: Comp[Seq[RecordFieldC & HasContext[ctx.type]]] =
        fieldsCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            scope2 = context.Scopes.ParameterScope(context.TRExprContext.ParameterOwner.Rec(this), scope, sig.parameters)
            fields <- ZIO.foreach(decl.body.collect { case WithLocation(field: ast.RecordField, loc) => field }) { field =>
                val owningRec = this
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

                for
                  fieldId <- UniqueIdentifier.make
                  t <- tr.typeCheckExpr(scope2)(field.fieldType, sig.returnType, context.DefaultExprContext.EffectInfo.Pure)
                yield new RecordFieldC {
                  override val context: ctx.type = ctx
                  override val id: UniqueIdentifier = fieldId

                  override def owningRecord: ArRecord = owningRec

                  override val name: IdentifierExpr = field.name.value
                  override val fieldType: context.DefaultExprContext.Expr = t
                }
            }
          yield fields
        )

      override def toString(): String =
        decl.name.toString()
    }
}
