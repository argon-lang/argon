package dev.argon.source

import dev.argon.ast
import dev.argon.ast.Modifier
import dev.argon.compiler.*
import dev.argon.util.*
import zio.*
import zio.prelude.NonEmptyMap

private[source] object SourceTrait {
  def make
  (ctx: Context)
  (closure: DeclarationClosure & HasContext[ctx.type])
  (decl: ast.TraitDeclarationStmt)
  : ctx.Comp[DeclarationResult[closure.Access, ArTraitC & HasContext[ctx.type]]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      methodsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[MethodEntry[ctx.type]]]

      mp <- ModifierParser.make(decl.modifiers, decl.name.location)
      access <- mp.parse(closure.accessModifierParser)
      _ <- mp.done

    yield DeclarationResult(
      access,
      new ArTraitC {
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
          SourceSignature.parse(ctx)(scope)(closure.accessToken)(context.TRExprContext.ExpressionOwner.Trait(this))(decl.parameters, rt)
        }



        override def methods: Comp[Seq[MethodEntry[context.type]]] =
          methodsCache.get(
            signature.map { sig =>
                context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Trait(this), closure.scope, sig.parameters)
              }
              .flatMap { scope2 =>
                val thisTrait = this
                val methodClosure = new MethodClosure {
                  override val context: ctx.type = ctx

                  override type Access = AccessModifier

                  override def methodOwner: MethodOwner[context.type] =
                    MethodOwner.ByTrait(thisTrait)

                  override def accessModifierParser: NonEmptyMap[Set[Modifier], Access] =
                    ModifierParser.accessModifierMember

                  override def scope: context.Scopes.Scope =
                    scope2

                  override def accessToken: AccessToken & HasContext[context.type] =
                    closure.accessToken.add(thisTrait)
                }
                ZIO.foreach(decl.body.collect { case WithLocation(method: ast.MethodDeclarationStmt, _) => method }) { methodDecl =>
                  for
                    res <- SourceMethod.make(ctx)(methodClosure)(methodDecl)
                  yield MethodEntry(res.access, res.declaration)
                }
              }

          )

        override def toString(): String =
          decl.name.toString()
      },
    )
}
