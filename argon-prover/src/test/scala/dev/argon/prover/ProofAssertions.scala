package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.test.Assertion.*
import zio.test.*

trait ProofAssertions[R <: VariableProvider, E] extends ProofPrologContext[R, E] {

  def hasProof(proof: Proof[String]): Assertion[PrologResult] =
    Assertion.assertion[PrologResult]("hasProof") {
      case PrologResult.Yes(p, _) => proof == p
      case PrologResult.Unknown | PrologResult.No(_, _) => false
    }

  def disproved(proof: Assertion[Proof[String]]): Assertion[PrologResult] =
    Assertion.assertionRec[PrologResult, Proof[String]]("disproved")(proof) {
      case PrologResult.No(p, _) => Some(p)
      case PrologResult.Unknown | PrologResult.Yes(_, _) => None
    }

  def notProven: Assertion[PrologResult] =
    Assertion.assertion[PrologResult]("notProven") {
      case PrologResult.No(_, _) | PrologResult.Unknown => true
      case PrologResult.Yes(_, _) => false
    }

}
