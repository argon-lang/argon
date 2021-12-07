package dev.argon.prover

trait PrologSyntax {
    type TVariable
    type TConstructor
    type TPredicateFunction

    given variableCanEqual: CanEqual[TVariable, TVariable]
    given constructorCanEqual: CanEqual[TConstructor, TConstructor]
    given predicateFunctionCanEqual: CanEqual[TPredicateFunction, TPredicateFunction]

    sealed trait Expr derives CanEqual
    final case class Value(constructor: TConstructor, args: Seq[Expr]) extends Expr
    final case class Variable(variable: TVariable) extends Expr

    sealed trait Predicate derives CanEqual
    final case class PredicateFunction(function: TPredicateFunction, args: Seq[Expr]) extends Predicate
    final case class And(a: Predicate, b: Predicate) extends Predicate
    final case class Or(a: Predicate, b: Predicate) extends Predicate
    final case class Implies(a: Predicate, b: Predicate) extends Predicate
    case object PropFalse extends Predicate

}
