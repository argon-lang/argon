package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.*
import zio.stream.{Stream, ZStream}

abstract class ProofPrologContext[R <: VariableProvider, E]
    extends PrologContext[R, E] with CommonProofRelations[R, E] {
  override val syntax: SimplePrologSyntaxBase
  import syntax.*

  override type ProofAtom = String

  protected override def variableIsFromRules(variable: String): UIO[Boolean] = ZIO.succeed(true)

  protected override def intrinsicPredicate
    (predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, solveState: SolveState)
    : ZStream[R, Either[E, PrologResult.No], PrologResult.Yes] = ZStream.empty

  protected override def normalize(expr: Value, substitutions: Model, solveState: SolveState): ZIO[R, E, Expr] = ZIO.succeed(expr)

  protected override def variableRelationProof(relation: Unit, a: String, b: String): ZIO[R, E, Proof[String]] =
    ZIO.succeed(Proof.Atomic("built-in-equal"))

  protected override def variableExprRelationProof(relation: Unit, a: String, b: syntax.Expr)
    : ZIO[R, E, Proof[String]] = ZIO.succeed(Proof.Atomic("built-in-equal"))

  protected override def valueRelationProof
    (relation: Unit, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[String]])
    : ZIO[R, E, Proof[String]] = ZIO.succeed(Proof.Atomic("built-in-equal"))

}
