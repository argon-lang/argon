package dev.argon.prover.smt

import dev.argon.prover.*
import zio.test.*
import zio.test.Assertion.*

trait ProofAssertions[R <: VariableProvider, E] extends TestSmtContext[R, E] {

  def proven: Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("hasProof") {
      case ProofResult.Yes(_, _) => true
      case ProofResult.Unknown | ProofResult.No(_, _) => false
    }

  def notProven: Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("notProven") {
      case ProofResult.No(_, _) | ProofResult.Unknown => true
      case ProofResult.Yes(_, _) => false
    }

}
