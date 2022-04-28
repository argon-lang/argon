package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.*
import zio.stream.{Stream, ZStream}

abstract class ProofPrologContext[R <: VariableProvider, E]
    extends PrologContext[R, E] with CommonProofRelations[R, E] {
  override val syntax: SimplePrologSyntaxBase
  import syntax.*

  override type ProofAtom = String

  protected override def variableIsFromRules(variable: String): UIO[Boolean] = IO.succeed(true)

  protected override def intrinsicPredicate
    (predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, fuel: Int)
    : ZStream[R, Error, PrologResult.Yes] = Stream.empty

  protected override def normalize(expr: Expr, fuel: Int): ZIO[R, E, Expr] = IO.succeed(expr)

  protected override def variableRelationProof(relation: Unit, a: String, b: String): ZIO[R, E, Proof[String]] =
    IO.succeed(Proof.Atomic("built-in-equal"))

  protected override def variableExprRelationProof(relation: Unit, a: String, b: syntax.Expr)
    : ZIO[R, E, Proof[String]] = IO.succeed(Proof.Atomic("built-in-equal"))

  protected override def valueRelationProof
    (relation: Unit, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[String]])
    : ZIO[R, E, Proof[String]] = IO.succeed(Proof.Atomic("built-in-equal"))

}
