package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.*
import zio.test.Assertion.*
import zio.test.*

object PrologContradictionProofTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object A extends TestPredicate derives CanEqual
  case object B extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual {
    def absurd: Nothing
  }

  val prologSyntax = new SimplePrologSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  override def spec: Spec[Environment & Scope, Any] =
    suite("Contradiction Proofs")(
      test("A or B, not A, not B") {
        val prologContext =
          new ProofPrologContext[VariableProvider, Nothing]
            with ProofAssertions[VariableProvider, Nothing] {
            override val syntax: prologSyntax.type = prologSyntax

            protected override def assertions: URIO[VariableProvider, List[(Proof[String], syntax.Predicate)]] =
              ZIO.succeed(List(
                Proof.Atomic("disjunct") -> Or(pred(A), pred(B)),
                Proof.Atomic("nota") -> Implies(pred(A), PropFalse),
                Proof.Atomic("notb") -> Implies(pred(B), PropFalse),
              ))
          }

        import prologContext.hasProof

        assertZIO(prologContext.check(PropFalse, fuel))(
          hasProof(Proof.Contradiction(
            Proof.Atomic("disjunct"),
            Proof.DeMorganAndPullNotOut(Proof.ConjunctIntro(Proof.Atomic("nota"), Proof.Atomic("notb"))),
          ))
        )
      }
    ).provideSome[Environment](VariableProvider.live)

}
