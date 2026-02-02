package dev.argon.expr

import cats.*
import cats.data.State
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}

trait ExprComparer extends TreeComparison {
  import StandardComparers.given

  val exprContext: ExprContext
  import exprContext.*


  protected def exprComparer: Comparer[Expr] = autoComparer
  protected def patternComparer: Comparer[Pattern] = autoComparer
  protected def holeComparer: Comparer[Hole] = equalComparer

  protected given Comparer[Expr] = exprComparer
  protected given Comparer[Pattern] = patternComparer
  protected given Comparer[Hole] = holeComparer
  
  private given Comparer[Builtin] = autoComparer
  private given Comparer[LocalVar] = autoComparer
  private given Comparer[Var] = autoComparer

  private given Comparer[Expr.RecordType] = autoComparer
  private given Comparer[Expr.EnumType] = autoComparer
  private given Comparer[Expr.TraitType] = autoComparer
  
  private given Comparer[MethodInstanceType]:
    override def compare(a: MethodInstanceType, b: MethodInstanceType): Comparison =
      exprComparer.compare(a, b)
  end given

  private given Comparer[RecordFieldLiteral] = autoComparer
  private given Comparer[RecordFieldPattern] = autoComparer
  private given Comparer[MatchCase] = autoComparer
  private given Comparer[ExpressionOwner] = equalComparer

  private given Comparer[Function] = equalComparer
  private given Comparer[Record] = equalComparer
  private given Comparer[RecordField] = equalComparer
  private given Comparer[Enum] = equalComparer
  private given Comparer[EnumVariant] = equalComparer
  private given Comparer[Trait] = equalComparer
  private given Comparer[Method] = equalComparer
  private given Comparer[Instance] = equalComparer

  private given Comparer[NullaryBuiltin] = equalComparer
  private given Comparer[UnaryBuiltin] = equalComparer
  private given Comparer[BinaryBuiltin] = equalComparer

  private given Comparer[UniqueIdentifier] = equalComparer
  private given Comparer[IdentifierExpr] = equalComparer
  private given [E <: ErasureMode] => Comparer[E] = equalComparer
}
