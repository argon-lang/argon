package dev.argon.prover.smt

import dev.argon.prover.*
import zio.*
import zio.stream.*

trait CommonProofRelations[R <: VariableProvider, E] extends SmtContext[R, E] {
  override val syntax: SimpleProverSyntaxBase
  import syntax.*

  override type TRelation = Unit
  override type TConstraints = syntax.Expr

  protected override def otherForEquivalenceRelation(constraints: syntax.Expr): Option[syntax.Expr] = Some(constraints)
}
