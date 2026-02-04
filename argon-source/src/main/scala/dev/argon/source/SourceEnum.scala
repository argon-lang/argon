package dev.argon.source

import dev.argon.ast
import dev.argon.ast.{EnumVariant, IdentifierExpr}
import dev.argon.compiler.*
import dev.argon.util.*
import zio.*

private[source] object SourceEnum {
  def make
  (ctx: Context)
  (closure: DeclarationClosure & HasContext[ctx.type])
  (decl: ast.EnumDeclarationStmt)
  : ctx.Comp[DeclarationResult[closure.Access, ArEnumC & HasContext[ctx.type]]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      variantsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[EnumVariantC & HasContext[ctx.type]]]
      
      mp <- ModifierParser.make(decl.modifiers, decl.name.location)
      access <- mp.parse(closure.accessModifierParser)
      _ <- mp.done
      
    yield DeclarationResult(
      access,
      new ArEnumC {
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
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Enum(this))(decl.parameters, rt)
        }
  
        override def variants: Comp[Seq[EnumVariant]] =
          variantsCache.get(
            for
              scope = closure.scope
              variants <- ZIO.foreach(decl.body.collect { case WithLocation(variantDecl: ast.EnumVariant, _) => variantDecl }) { variantDecl =>
                  val ownEnum = this
                  for
                    fieldId <- UniqueIdentifier.make
                    sigCache <- MemoCell.make[ctx.Env, ctx.Error, FunctionSignature]
                    fieldsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[RecordField]]
                  yield new EnumVariantC {
                    override val context: ctx.type = ctx
                    override val id: UniqueIdentifier = fieldId
  
                    override def owningEnum: ArEnum = ownEnum
  
                    override val name: IdentifierExpr = variantDecl match {
                      case variantDecl: EnumVariant.Constructor => variantDecl.name.value
                      case EnumVariant.Record(record) => record.name.value
                    }
  
                    override def signature: Comp[FunctionSignature] = sigCache.get {
                      val (params, returnType) = variantDecl match {
                        case variantDecl: EnumVariant.Constructor =>
                          (variantDecl.parameters, variantDecl.returnType)
  
                        case EnumVariant.Record(record) =>
                          (record.parameters, record.returnType)
                      }
                      
                      val paramOwner = context.TRExprContext.ExpressionOwner.EnumVariant(this)
  
                      returnType match {
                        case Some(returnTypeExpr) =>
                          val rt = SourceSignature.exprToReturnType(returnTypeExpr)
                          SourceSignature.parse(ctx)(scope)(paramOwner)(decl.parameters, rt)
  
                        case None =>
                          val fakeRT = SourceSignature.getTypeSigReturnType(decl.name, decl.returnType)
                          SourceSignature.parse(ctx)(scope)(paramOwner)(decl.parameters ++ params, fakeRT)
                            .map { sig =>
                              sig.copy(
                                returnType = context.DefaultExprContext.Expr.EnumType(
                                  owningEnum,
                                  SignatureParameter.getParameterVariables(
                                    context.DefaultExprContext.ExpressionOwner.EnumVariant(this),
                                    sig.parameters.take(decl.parameters.size)
                                  )
                                    .map(context.DefaultExprContext.Expr.Variable.apply)
                                )
                              )
                            }
                      }
                    }
  
                    override def fields: Comp[Seq[RecordField]] =
                      fieldsCache.get(
                        for
                          sig <- signature
                          scope2 = context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.EnumVariant(this), scope, sig.parameters)
                          
                          body = variantDecl match {
                            case _: EnumVariant.Constructor => Seq.empty
                            case EnumVariant.Record(record) => record.body
                          }
                          
                          fields <- ZIO.foreach(
                            body.collect { case WithLocation(field: ast.RecordField, _) => field }
                          ) { field =>
                            SourceRecordField(context, scope2, sig, this)(field)
                          }
                        yield fields
                      )
                      
                  }
              }
            yield variants
  
          )
  
        override def toString(): String =
          decl.name.toString()
      },
    )
}
