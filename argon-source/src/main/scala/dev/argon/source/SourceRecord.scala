package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.util.*
import zio.*

private[source] object SourceRecord {
  def make
  (ctx: Context)
  (closure: DeclarationClosure & HasContext[ctx.type])
  (decl: ast.RecordDeclarationStmt)
  : ctx.Comp[DeclarationResult[closure.Access, ArRecordC & HasContext[ctx.type]]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      fieldsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[RecordFieldC & HasContext[ctx.type]]]
      
      mp <- ModifierParser.make(decl.modifiers, decl.name.location)
      access <- mp.parse(closure.accessModifierParser)
      _ <- mp.done
      
    yield DeclarationResult(
      access,
      new ArRecordC {
        override val context: ctx.type = ctx
        override val id: UniqueIdentifier = recId
  
        override def importSpecifier: Comp[ImportSpecifier] =
          for
            sig <- signature
            erasedSig <- SignatureEraser(ctx).eraseSignature(sig)
          yield closure.getImportSpecifier(erasedSig)
  
        override def signature: Comp[FunctionSignature] = sigCache.get {
          val scope = closure.scope
          val rt = SourceSignature.getTypeSigReturnType(decl.name, decl.returnType)
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Rec(this))(decl.parameters, rt)
        }
  
        override def fields: Comp[Seq[RecordField]] =
          fieldsCache.get(
            for
              scope = closure.scope
              sig <- signature
              scope2 = context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Rec(this), scope, sig.parameters)
              fields <- ZIO.foreach(
                decl.body.collect { case WithLocation(field: ast.RecordField, _) => field }
              ) { field =>
                SourceRecordField(context, scope2, sig, this)(field)
              }
            yield fields
          )
  
        override def toString(): String =
          decl.name.toString()
      }
    )
}
