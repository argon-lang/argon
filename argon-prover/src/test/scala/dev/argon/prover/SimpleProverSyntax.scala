package dev.argon.prover

import dev.argon.prover.*
import dev.argon.util.TreeComparison

class SimpleProverSyntax[PredFunc, Constructor] extends ProverSyntax {
  override type TVariable = Expr.Variable
  override type TPredicateExpr = PredicateApply

  enum Expr {
    case Variable(name: String)
    case Value(constructor: Constructor, args: Seq[Expr])
  }

  final case class PredicateApply(name: PredFunc, args: Seq[Expr])
  
  def not(p: Predicate): Predicate = Implies(p, PropFalse)

  def expr(ctor: Constructor, args: Expr*): Expr = Expr.Value(ctor, args)

  def pred(predicateFunction: PredFunc, args: Expr*): Predicate = PredicateExpression(PredicateApply(predicateFunction, args))

  extension(pred: Predicate) {
    def &(other: Predicate): Predicate = And(pred, other)
    def |(other: Predicate): Predicate = Or(pred, other)
    def unary_! : Predicate = not(pred)
    def ==>(other: Predicate): Predicate = Implies(pred, other)
  }

}
