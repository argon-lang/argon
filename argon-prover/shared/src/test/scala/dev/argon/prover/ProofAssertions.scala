package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.test.Assertion.*
import zio.test.*
import zio.test.Assertion.Render.*

trait ProofAssertions[R <: VariableProvider & zio.Random, E] extends ProofPrologContext[R, E] {

  def hasProof(proof: Proof[String]): Assertion[PrologResult] =
    Assertion.assertion[PrologResult]("hasProof")(param(proof)) {
      case PrologResult.Yes(p, _) => proof == p
      case PrologResult.Unknown | PrologResult.No(_, _) => false
    }

  def disproved(proof: Assertion[Proof[String]]): Assertion[PrologResult] =
    Assertion.assertionRec[PrologResult, Proof[String]]("disproved")()(proof) {
      case PrologResult.No(p, _) => Some(p)
      case PrologResult.Unknown | PrologResult.Yes(_, _) => None
    }

}
