package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.util.*
import zio.*

private[source] object SourceInstance {
  def make
  (ctx: Context)
  (closure: DeclarationClosure & HasContext[ctx.type])
  (decl: ast.InstanceDeclarationStmt)
  : ctx.Comp[DeclarationResult[closure.Access, ArInstanceC & HasContext[ctx.type]]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      methodsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[ArMethodC & HasContext[ctx.type]]]

      mp <- ModifierParser.make(decl.modifiers, decl.name.location)
      access <- mp.parse(closure.accessModifierParser)
      _ <- mp.done

    yield DeclarationResult(
      access,
      new ArInstanceC {
        override val context: ctx.type = ctx
        override val id: UniqueIdentifier = recId
        
        import closure.given
  
        override def importSpecifier: Comp[ImportSpecifier] =
          for
            sig <- signature
            erasedSig <- SignatureEraser(ctx).eraseSignature(sig)
          yield closure.getImportSpecifier(erasedSig)
  
        override def signature: Comp[FunctionSignature] = sigCache.get {
          val scope = closure.scope
          val rt = SourceSignature.getTypeSigReturnType(decl.name, decl.returnType)
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Instance(this))(decl.parameters, rt)
        }
  
        override def methods: Comp[Seq[ArMethod]] =
          methodsCache.get(
            signature.map { sig =>
              context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Instance(this), closure.scope, sig.parameters)
            }
              .flatMap { scope2 =>
                ZIO.foreach(decl.body.collect { case WithLocation(method: ast.MethodDeclarationStmt, _) => method })(
                  SourceMethod.make(ctx)(scope2, MethodOwner.ByInstance(this))
                )
              }
          )
  
        override def toString(): String =
          decl.name.toString()
      },
    )
}
