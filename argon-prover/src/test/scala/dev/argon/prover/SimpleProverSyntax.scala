package dev.argon.prover

import dev.argon.prover.*
import dev.argon.util.TreeComparison

class SimpleProverSyntax[Constructor] extends ProverSyntax {
  override type TVariable = Expr.Variable
  override type TPredicateExpr = Expr

  override def variableToExpr(v: Expr.Variable): Expr = v
  

  enum Expr {
    case Variable(name: String)
    case Value(constructor: Constructor, args: Seq[Expr])
  }

  def expr(ctor: Constructor, args: Expr*): Expr = Expr.Value(ctor, args)

  def pred(ctor: Constructor, args: Expr*): Predicate = PredicateExpression(expr(ctor, args*))

  extension(pred: Predicate) {
    def &(other: Predicate): Predicate = And(pred, other)
    def |(other: Predicate): Predicate = Or(pred, other)
    def unary_! : Predicate = Implies(pred, PropFalse)
    def ==>(other: Predicate): Predicate = Implies(pred, other)
  }

}
