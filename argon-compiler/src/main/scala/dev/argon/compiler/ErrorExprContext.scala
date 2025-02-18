package dev.argon.compiler

import dev.argon.expr.ExprContext
import dev.argon.util.UniqueIdentifier
import dev.argon.ast.IdentifierExpr

object ErrorExprContext extends ExprContext {
  override type Function = ArFuncC
  override type Record = ArRecordC

  override type RecordField = RecordFieldC
  override def getRecordFieldName(f: RecordFieldC): IdentifierExpr = f.name

  override type Hole = UniqueIdentifier

}
