package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.{ZIO, IO, URIO}
import zio.test.Assertion._
import zio.test._

object PrologContradictionProofTests extends DefaultRunnableSpec {

  sealed trait TestPredicate derives CanEqual
  case object A extends TestPredicate derives CanEqual
  case object B extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual {
    def absurd: Nothing
  }

  val prologSyntax = new SimplePrologSyntax[TestPredicate, TestCtor]
  import prologSyntax._

  val fuel = 100

  override def spec: ZSpec[Environment, Failure] =
    suite("Contradiction Proofs")(
      testM("A or B, not A, not B") {
        val prologContext =
          new ProofPrologContext[VariableProvider & zio.Random, Nothing]
            with ProofAssertions[VariableProvider & zio.Random, Nothing] {
            override val syntax: prologSyntax.type = prologSyntax

            protected override def assertions: URIO[VariableProvider, List[(Proof[String], syntax.Predicate)]] =
              IO.succeed(List(
                Proof.Atomic("disjunct") -> Or(pred(A), pred(B)),
                Proof.Atomic("nota") -> Implies(pred(A), PropFalse),
                Proof.Atomic("notb") -> Implies(pred(B), PropFalse),
              ))
          }

        import prologContext.hasProof

        assertM(prologContext.check(PropFalse, fuel))(
          hasProof(Proof.Contradiction(
            Proof.Atomic("disjunct"),
            Proof.DeMorganAndPullNotOut(Proof.ConjunctIntro(Proof.Atomic("nota"), Proof.Atomic("notb"))),
          ))
        )
      }
    ).provideCustomLayer(VariableProvider.live)

}
