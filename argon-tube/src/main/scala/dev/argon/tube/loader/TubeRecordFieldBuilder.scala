package dev.argon.tube.loader

import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.{HasContext, RecordFieldC, UsingContext}
import dev.argon.tube.RecordFieldDefinition
import dev.argon.util.UniqueIdentifier

trait TubeRecordFieldBuilder extends UsingContext with LoaderUtils {  
  def createRecordField(rec: ArRecord | EnumVariant)(fieldDef: RecordFieldDefinition): Comp[RecordField] =
    for
      fieldId <- UniqueIdentifier.make
      t <- decodeExpr(fieldDef.fieldType)
    yield new RecordFieldC {

      override val context: TubeRecordFieldBuilder.this.context.type = TubeRecordFieldBuilder.this.context

      override def owningRecord: ArRecord | EnumVariant = rec

      override val id: UniqueIdentifier = fieldId
      override val isMutable: Boolean = fieldDef.mutable
      override val name: IdentifierExpr = decodeIdentifier(fieldDef.name)
      override val fieldType: context.DefaultExprContext.Expr = t

    }
}
