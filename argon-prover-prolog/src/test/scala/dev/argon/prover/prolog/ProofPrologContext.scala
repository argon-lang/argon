package dev.argon.prover.prolog

import dev.argon.prover.prolog.SimplePrologContext.VariableProvider
import dev.argon.prover.*
import zio.*
import zio.stream.{Stream, ZStream}

abstract class ProofPrologContext[R <: VariableProvider, E]
    extends PrologContext[R, E] with CommonProofRelations[R, E] {
  override val syntax: SimpleProverSyntaxBase
  import syntax.*

  override type ProofAtom = String

  protected override def variableIsFromRules(variable: String): UIO[Boolean] = ZIO.succeed(true)

  protected override def intrinsicPredicate
    (predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, solveState: SolveState)
    : ZStream[R, Either[E, ProofResult.No], ProofResult.Yes] = ZStream.empty

  protected override def normalize(expr: Value, substitutions: Model, fuel: Int): ZIO[R, E, Expr] = ZIO.succeed(expr)

  protected override def variableRelationProof(relation: Unit, a: String, b: String): ZIO[R, E, Proof[String]] =
    ZIO.succeed(Proof.Atomic("built-in-equal"))

  protected override def variableExprRelationProof(relation: Unit, a: String, b: syntax.Expr)
    : ZIO[R, E, Proof[String]] = ZIO.succeed(Proof.Atomic("built-in-equal"))

  protected override def valueRelationProof
    (relation: Unit, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[String]])
    : ZIO[R, E, Proof[String]] = ZIO.succeed(Proof.Atomic("built-in-equal"))

}
