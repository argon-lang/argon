package dev.argon.expr

object ValueUtil {
  
  def isValue(exprContext: ExprContext)(expr: exprContext.Expr): Boolean =
    import exprContext.*
    expr match
      case Expr.Error() => false
      case Expr.ErasedValue() => false
      case Expr.Hole(_) => false
      case Expr.AnyType() => true
      case Expr.BindVariable(_, _) => false
      case Expr.BoolLiteral(_) => true
      case Expr.Builtin(_) => true
      case Expr.Finally(_, _) => false
      case Expr.FunctionCall(_, _) => false
      case Expr.FunctionObjectCall(_, _) => false
      case Expr.FunctionType(_, _) => true
      case Expr.IfElse(_, _, _, _, _) => false
      case Expr.IntLiteral(_) => true
      case Expr.Lambda(_, _, _) => true
      case Expr.RecordType(_, _) => true
      case Expr.RecordFieldLoad(_, _, _) => false
      case Expr.RecordFieldStore(_, _, _, _) => false
      case Expr.RecordLiteral(_, _) => true
      case Expr.Sequence(_, _) => false
      case Expr.StringLiteral(_) => true
      case Expr.Tuple(_) => true
      case Expr.TupleElement(_, _) => false
      case Expr.TypeN(_) => true
      case Expr.TypeBigN(_) => true
      case Expr.Variable(_) => false
      case Expr.VariableStore(_, _) => false
    end match
  end isValue

}
