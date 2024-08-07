package dev.argon.prover.prolog

import dev.argon.prover.*
import dev.argon.prover.prolog.SimplePrologContext.VariableProvider
import zio.*
import zio.stream.{Stream, ZStream}

abstract class SimplePrologContext[R <: VariableProvider, E]
    extends PrologContext[R, E] with SimpleProverContext[R, E] with CommonProofRelations[R, E] {
  import syntax.*


  protected final override def newVariable: ZIO[R, E, String] =
    VariableProvider.next

  protected override def variableIsFromRules(variable: String): UIO[Boolean] = ZIO.succeed(true)

  protected override def intrinsicPredicate
    (predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, solveState: SolveState)
    : ZStream[R, Either[E, ProofResult.No], ProofResult.Yes] = ZStream.empty

  protected override def variableRelationProof(relation: Unit, a: String, b: String): ZIO[R, E, Proof[Unit]] =
    ZIO.succeed(Proof.Atomic(()))

  protected override def variableExprRelationProof(relation: Unit, a: String, b: syntax.Expr): ZIO[R, E, Proof[Unit]] =
    ZIO.succeed(Proof.Atomic(()))

  protected override def valueRelationProof
    (relation: Unit, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[Unit]])
    : ZIO[R, E, Proof[Unit]] = ZIO.succeed(Proof.Atomic(()))

}

object SimplePrologContext {

  trait VariableProvider {
    def nextVariable: UIO[String]
  }

  object VariableProvider {

    def live: ULayer[VariableProvider] =
      ZLayer(for {
        nextId <- Ref.make(0)
      } yield new VariableProvider {

        override def nextVariable: UIO[String] =
          for {
            num <- nextId.updateAndGet(_ + 1)
          } yield s"#$num"

      })

    def next: URIO[VariableProvider, String] = ZIO.serviceWithZIO[VariableProvider](_.nextVariable)
  }

}
