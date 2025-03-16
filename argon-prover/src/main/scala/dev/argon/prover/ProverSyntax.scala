package dev.argon.prover

trait ProverSyntax {
  type TVariable
  type TPredicateExpr

  sealed trait Predicate derives CanEqual
  final case class PredicateExpression(e: TPredicateExpr) extends Predicate
  final case class And(a: Predicate, b: Predicate) extends Predicate
  final case class Or(a: Predicate, b: Predicate) extends Predicate
  final case class Implies(a: Predicate, b: Predicate) extends Predicate
  case object PropTrue extends Predicate
  case object PropFalse extends Predicate
  

  def variableToExpr(v: TVariable): TPredicateExpr

}
