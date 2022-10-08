package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*

trait SimpleProverContext[R, E] extends ProverContext[R, E] {
  override val syntax: SimpleProverSyntaxBase
  import syntax.*

  override type ProofAtom = Unit

  protected override def normalize(expr: Value, substitutions: Model, fuel: Int): ZIO[R, E, Expr] = ZIO.succeed(expr)
}
