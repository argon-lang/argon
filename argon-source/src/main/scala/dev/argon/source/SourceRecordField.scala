package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.{ArRecordC, Context, EnumVariantC, HasContext, RecordFieldC, TypeResolver}
import dev.argon.util.UniqueIdentifier

object SourceRecordField {
  def apply(
    ctx: Context,
    scope: ctx.Scopes.Scope,
    sig: ctx.DefaultSignatureContext.FunctionSignature,
    owningRec: (ArRecordC & HasContext[ctx.type]) | (EnumVariantC & HasContext[ctx.type]),
  )(field: ast.RecordField): ctx.Comp[RecordFieldC & HasContext[ctx.type]] =
    val tr = new TypeResolver {
      override val context: ctx.type = ctx
    }

    for
      fieldId <- UniqueIdentifier.make
      t <- tr.typeCheckTypeExprWithKind(scope)(field.fieldType, sig.returnType, erased = false)
    yield new RecordFieldC {
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = fieldId

      override def owningRecord: ArRecord | EnumVariant = owningRec

      override val isMutable: Boolean = field.isMutable
      override val name: IdentifierExpr = field.name.value
      override val fieldType: context.DefaultExprContext.Expr = t

      override def toString: String = s"Record Field $name"
    }
}
