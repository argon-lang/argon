package dev.argon.prover.smt

import dev.argon.prover.*
import zio.*
import zio.stream.{Stream, ZStream}

abstract class TestSmtContext[R <: VariableProvider, E]
    extends SmtContext[R, E] with CommonProofRelations[R, E] {
  override val syntax: SimpleProverSyntaxBase
  import syntax.*

  override type ProofAtom = String


  override protected def assumeResultProof: Proof[String] = Proof.Atomic("dummy")

  protected final override def newVariable: ZIO[R, E, String] =
    VariableProvider.next

  protected override def normalize(expr: Value, substitutions: Model, fuel: Int): ZIO[R, E, Expr] = ZIO.succeed(expr)
}
