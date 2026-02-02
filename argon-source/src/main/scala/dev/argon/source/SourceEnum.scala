package dev.argon.source

import dev.argon.ast
import dev.argon.ast.{EnumVariant, IdentifierExpr}
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[source] object 
SourceEnum {
  def make(ctx: Context)(scope: ctx.Scopes.GlobalScopeBuilder, importFactory: ImportFactory)(decl: ast.EnumDeclarationStmt): ctx.Comp[ArEnumC & HasContext[ctx.type]] =
    for
      recId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      variantsCache <- MemoCell.make[ctx.Env, ctx.Error, Seq[EnumVariantC & HasContext[ctx.type]]]
    yield new ArEnumC {
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
          SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Enum(this))(decl.parameters, rt)
        }
      )

      override def variants: Comp[Seq[EnumVariant]] =
        variantsCache.get(
          for
            scope <- scope.toScope
            sig <- signature
            variants <- ZIO.foreach(decl.body.collect { case WithLocation(variantDecl: ast.EnumVariant, loc) => variantDecl }) { variantDecl =>
                val ownEnum = this
                val tr = new TypeResolver {
                  override val context: ctx.type = ctx
                }

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
                          body.collect { case WithLocation(field: ast.RecordField, loc) => field }
                        ) { field =>
                          SourceRecordField(context, scope2, sig, this)(field)
                        }
                      yield fields
                    )
                    
                }
            }
          yield variants

        )


//      override def fields: Comp[Seq[RecordFieldC & HasContext[ctx.type]]] =
//        fieldsCache.get(
//          for
//            scope <- scope.toScope
//            sig <- signature
//            scope2 = context.Scopes.ParameterScope(context.TRExprContext.ParameterOwner.Enum(this), scope, sig.parameters)
//            fields <- ZIO.foreach(decl.body.collect { case WithLocation(field: ast.RecordField, loc) => field }) { field =>
//                val owningEnum = this
//                val tr = new TypeResolver {
//                  override val context: ctx.type = ctx
//                }
//
//                for
//                  fieldId <- UniqueIdentifier.make
//                  t <- tr.typeCheckExpr(scope2)(field.fieldType, sig.returnType, context.DefaultExprContext.EffectInfo.Pure, erased = true)
//                yield new RecordFieldC {
//                  override val context: ctx.type = ctx
//                  override val id: UniqueIdentifier = fieldId
//
//                  override def owningRecord: ArRecord = owningEnum
//
//                  override val isMutable: Boolean = field.isMutable
//                  override val name: IdentifierExpr = field.name.value
//                  override val fieldType: context.DefaultExprContext.Expr = t
//                }
//            }
//          yield fields
//        )

      override def toString(): String =
        decl.name.toString()
    }
}
