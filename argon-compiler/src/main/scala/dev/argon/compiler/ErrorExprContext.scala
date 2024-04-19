package dev.argon.compiler

import dev.argon.expr.ExprContext
import dev.argon.util.UniqueIdentifier
import dev.argon.ast.IdentifierExpr

object ErrorExprContext extends ExprContext {
  override type Function = ArFuncC
  override def functionCanEqual: CanEqual[Function, Function] = summon

  override type Record = ArRecordC
  override def recordCanEqual: CanEqual[Record, Record] = summon

  override type RecordField = RecordFieldC
  override def recordFieldCanEqual: CanEqual[RecordField, RecordField] = summon
  override def getRecordFieldName(f: RecordFieldC): IdentifierExpr = f.name

  override type Hole = UniqueIdentifier
  override def holeCanEqual: CanEqual[Hole, Hole] = summon

}
