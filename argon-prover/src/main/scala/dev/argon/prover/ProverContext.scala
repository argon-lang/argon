package dev.argon.prover


import dev.argon.util.Fuel
import zio.*

trait ProverContext[R, E] {
  val syntax: ProverSyntax
  import syntax.*

  type ProofAtom

  type Model

  sealed trait ProofResult derives CanEqual

  object ProofResult {
    sealed trait Definitive extends ProofResult

    final case class Yes(proof: Proof[ProofAtom], model: Model) extends Definitive
    final case class No(notProof: Proof[ProofAtom], model: Model) extends Definitive
    case object Unknown extends ProofResult
  }


  protected def newVariable: ZIO[R, E, TVariable]
  protected def freshAssertions(model: Model): Seq[ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]]
  protected def unifyPredicateExpression(f1: TPredicateExpr, f2: TPredicateExpr, model: Ref[Model], fuel: Fuel): ZIO[R, E, Boolean]

  def check(goal: Predicate, model: Model, fuel: Fuel): ZIO[R, E, ProofResult]
  
}
