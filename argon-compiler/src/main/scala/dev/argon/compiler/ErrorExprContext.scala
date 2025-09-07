package dev.argon.compiler

import dev.argon.expr.{ExprContext, ExprContextWithHoles}
import dev.argon.util.UniqueIdentifier
import dev.argon.ast.IdentifierExpr

object ErrorExprContext extends ExprContext with ExprContextWithHoles {
  override type Function = ArFuncC

  override type Record = ArRecordC
  override type RecordField = RecordFieldC
  override def getRecordFieldName(f: RecordFieldC): IdentifierExpr = f.name

  override type Enum = ArEnumC
  override type EnumVariant = EnumVariantC
}
