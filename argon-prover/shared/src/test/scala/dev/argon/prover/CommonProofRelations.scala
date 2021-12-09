package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio._
import zio.stream._

trait CommonProofRelations[R <: VariableProvider & Random, E] extends PrologContext[R, E] {
  override val syntax: SimplePrologSyntaxBase
  import syntax._

  override type TRelation = Unit
  override type TConstraints = syntax.Expr

  protected override def mergeRelations(parentExprRelation: Unit, subExprRelation: Unit): Unit = ()

  protected override def swapRelation(relation: Unit): Unit = ()

  protected override def createConstraints(relation: Unit, other: syntax.Expr): syntax.Expr = other

  protected override def createEqualityConstraint(other: syntax.Expr): syntax.Expr = other

  protected override def mergeConstraints(a: syntax.Expr, b: syntax.Expr): Option[syntax.Expr] = None

  protected override def predicateArgRelations(predicate: syntax.TPredicateFunction, arity: Int): UIO[Seq[Unit]] =
    IO.succeed(Seq.fill(arity)(()))

  protected override def constructorArgRelations(constructor: syntax.TConstructor, arity: Int): UIO[Seq[Unit]] =
    IO.succeed(Seq.fill(arity)(()))

  protected override def checkRelation
    (a: syntax.Expr, b: syntax.Expr, relation: TRelation, substitutions: Model, fuel: Int)
    : ZStream[R, Error, PrologResult.Yes] = Stream.empty

  protected override def otherForEquivalenceRelation(constraints: syntax.Expr): Option[syntax.Expr] = Some(constraints)
}
