package dev.argon.prover


import zio.*
import zio.stream.*

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
  protected def freshAssertions: Seq[ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]]

  def check(goal: Predicate, model: Model, fuel: Int): ZIO[R, E, ProofResult]
  
}
