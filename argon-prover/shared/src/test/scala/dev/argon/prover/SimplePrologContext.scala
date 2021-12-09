package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio._
import zio.stream.{Stream, ZStream}

abstract class SimplePrologContext[R <: VariableProvider & Random, E]
    extends PrologContext[R, E] with CommonProofRelations[R, E] {
  override val syntax: SimplePrologSyntaxBase
  import syntax._

  override type ProofAtom = Unit

  protected override def variableIsFromRules(variable: String): UIO[Boolean] = IO.succeed(true)

  protected override def intrinsicPredicate
    (predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, fuel: Int)
    : ZStream[R, Error, PrologResult.Yes] = Stream.empty

  protected override def normalize(expr: Expr, fuel: Int): ZIO[R, E, Expr] = IO.succeed(expr)

  protected override def variableRelationProof(relation: Unit, a: String, b: String): ZIO[R, E, Proof[Unit]] =
    IO.succeed(Proof.Atomic(()))

  protected override def variableExprRelationProof(relation: Unit, a: String, b: syntax.Expr): ZIO[R, E, Proof[Unit]] =
    IO.succeed(Proof.Atomic(()))

  protected override def valueRelationProof
    (relation: Unit, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[Unit]])
    : ZIO[R, E, Proof[Unit]] = IO.succeed(Proof.Atomic(()))

}

object SimplePrologContext {

  trait VariableProvider {
    def nextVariable: UIO[String]
  }

  object VariableProvider {

    def live: ULayer[VariableProvider] =
      ZLayer.fromEffect(for {
        nextId <- Ref.make(0)
      } yield new VariableProvider {

        override def nextVariable: UIO[String] =
          for {
            num <- nextId.updateAndGet(_ + 1)
          } yield s"#$num"

      })

    def next: URIO[VariableProvider, String] = ZIO.accessM[VariableProvider](_.get.nextVariable)
  }

}
