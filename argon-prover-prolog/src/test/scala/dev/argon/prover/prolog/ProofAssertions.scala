package dev.argon.prover.prolog

import dev.argon.prover.*
import dev.argon.prover.prolog.SimplePrologContext.VariableProvider
import zio.test.Assertion.*
import zio.test.*

trait ProofAssertions[R <: VariableProvider, E] extends ProofPrologContext[R, E] {

  def hasProof(proof: Proof[String]): Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("hasProof") {
      case ProofResult.Yes(p, _) => proof == p
      case ProofResult.Unknown | ProofResult.No(_, _) => false
    }

  def disproved(proof: Assertion[Proof[String]]): Assertion[ProofResult] =
    Assertion.assertionRec[ProofResult, Proof[String]]("disproved")(proof) {
      case ProofResult.No(p, _) => Some(p)
      case ProofResult.Unknown | ProofResult.Yes(_, _) => None
    }

  def notProven: Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("notProven") {
      case ProofResult.No(_, _) | ProofResult.Unknown => true
      case ProofResult.Yes(_, _) => false
    }

}
