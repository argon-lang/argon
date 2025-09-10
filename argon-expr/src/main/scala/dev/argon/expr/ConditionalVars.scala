package dev.argon.expr

trait ConditionalVars {
  self: ExprContext =>
  
  final case class ConditionalVars(
    whenTrue: Set[LocalVar],
    whenFalse: Set[LocalVar],
  )
  
  def getConditionalVars(e: Expr): ConditionalVars =
    e match {
      case Expr.And(a, b) =>
        val av = getConditionalVars(a)
        val bv = getConditionalVars(b)
        ConditionalVars(
          whenTrue = av.whenTrue ++ bv.whenTrue,
          whenFalse = Set.empty,
        )

      case Expr.Or(a, b) =>
        val av = getConditionalVars(a)
        val bv = getConditionalVars(b)
        ConditionalVars(
          whenTrue = Set.empty,
          whenFalse = av.whenFalse ++ bv.whenFalse,
        )

      case Expr.Not(a) =>
        val av = getConditionalVars(a)
        ConditionalVars(
          whenTrue = av.whenFalse,
          whenFalse = av.whenTrue,
        )

      case Expr.Is(_, pattern) =>
        ConditionalVars(
          whenTrue = getPatternVars(pattern),
          whenFalse = Set.empty,
        )

      case _ => ConditionalVars(Set.empty, Set.empty)
    }
    
  def getPatternVars(pattern: Pattern): Set[LocalVar] =
    pattern match {
      case Pattern.Discard(_) => Set.empty
      case Pattern.Tuple(elements) => elements.flatMap(getPatternVars).toSet
      case Pattern.Binding(v, pattern) => getPatternVars(pattern) + v
      case Pattern.EnumVariant(_, _, args, fields) =>
        args.flatMap(getPatternVars).toSet ++
          fields.flatMap(field => getPatternVars(field.pattern))
      case Pattern.String(_) => Set.empty
      case Pattern.Int(_) => Set.empty
      case Pattern.Bool(_) => Set.empty
    }
  
}
