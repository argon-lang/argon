package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio._
import zio.stream._

trait CommonProofRelations[R <: VariableProvider & Random, E] extends PrologContext[R, E] {
  override val syntax: SimplePrologSyntaxBase
  import syntax._

  override type TRelation = Unit
  override type TConstraints = syntax.Expr


  override protected def mergeRelations(parentExprRelation: Unit, subExprRelation: Unit): Unit = ()

  override protected def swapRelation(relation: Unit): Unit = ()

  override protected def createConstraints(relation: Unit, other: syntax.Expr): syntax.Expr = other

  override protected def createEqualityConstraint(other: syntax.Expr): syntax.Expr = other

  override protected def mergeConstraints(a: syntax.Expr, b: syntax.Expr): Option[syntax.Expr] = None

  override protected def predicateArgRelations(predicate: syntax.TPredicateFunction, arity: Int): UIO[Seq[Unit]] = IO.succeed(Seq.fill(arity)(()))

  override protected def constructorArgRelations(constructor: syntax.TConstructor, arity: Int): UIO[Seq[Unit]] = IO.succeed(Seq.fill(arity)(()))

  override protected def checkRelation(a: syntax.Expr, b: syntax.Expr, relation: TRelation, substitutions: Model, fuel: Int): ZStream[R, Error, PrologResult.Yes] =
    Stream.empty

  override protected def otherForEquivalenceRelation(constraints: syntax.Expr): Option[syntax.Expr] = Some(constraints)
}
