package dev.argon.prover.smt

import dev.argon.prover.*

sealed trait SimpleProverSyntaxBase extends ProverSyntax {
  override type TVariable = String
  override type TConstructor
  override type TPredicateFunction
}

class SimpleProverSyntax[PredFunc, Constructor]
  (using
    override val constructorCanEqual: CanEqual[Constructor, Constructor],
    override val predicateFunctionCanEqual: CanEqual[PredFunc, PredFunc],
  ) extends SimpleProverSyntaxBase {
  override type TConstructor = Constructor
  override type TPredicateFunction = PredFunc

  override def variableCanEqual: CanEqual[TVariable, TVariable] = summon[CanEqual[String, String]]

  def not(p: Predicate): Predicate = Implies(p, PropFalse)

  def expr(ctor: TConstructor, args: Expr*): Expr = Value(ctor, args)

  def pred(predicateFunction: PredFunc, args: Expr*): Predicate = PredicateFunction(predicateFunction, args)

  extension(sc: StringContext) {
    def v(args: Any*): Variable = Variable(sc.s(args*))
  }

  extension(pred: Predicate) {
    def &(other: Predicate): Predicate = And(pred, other)
    def |(other: Predicate): Predicate = Or(pred, other)
    def unary_! : Predicate = not(pred)
    def ==>(other: Predicate): Predicate = Implies(pred, other)
  }

}
