package dev.argon.prover.prolog

import dev.argon.prover.*
import dev.argon.prover.prolog.SimplePrologContext.VariableProvider
import zio.*
import zio.stream.*

trait CommonProofRelations[R <: VariableProvider, E] extends PrologContext[R, E] {
  override val syntax: SimpleProverSyntaxBase
  import syntax.*

  override type TRelation = Unit
  override type TConstraints = syntax.Expr

  protected override def mergeRelations(parentExprRelation: Unit, subExprRelation: Unit): Unit = ()

  protected override def swapRelation(relation: Unit): Unit = ()

  protected override def createConstraints(relation: Unit, other: syntax.Expr): syntax.Expr = other

  protected override def createEqualityConstraint(other: syntax.Expr): syntax.Expr = other

  protected override def mergeConstraints(a: syntax.Expr, b: syntax.Expr): Option[syntax.Expr] = None

  protected override def predicateArgRelations(predicate: syntax.TPredicateFunction, arity: Int): UIO[Seq[Unit]] =
    ZIO.succeed(Seq.fill(arity)(()))

  protected override def constructorArgRelations(constructor: syntax.TConstructor, arity: Int): UIO[Seq[Unit]] =
    ZIO.succeed(Seq.fill(arity)(()))

  protected override def checkRelation
    (a: Expr, b: Expr, relation: TRelation, substitutions: Model, solveState: SolveState)
    : ZStream[R, Either[E, ProofResult.No], ProofResult.Yes] = ZStream.empty

  protected override def otherForEquivalenceRelation(constraints: syntax.Expr): Option[syntax.Expr] = Some(constraints)
}
