package dev.argon.prover.smt

import dev.argon.prover.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object SmtProofTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object EqualTo extends TestPredicate derives CanEqual
  case object Gt extends TestPredicate derives CanEqual
  case object KnownTrue extends TestPredicate derives CanEqual
  case object KnownFalse extends TestPredicate derives CanEqual
  final case class Constant(name: String) extends TestPredicate

  sealed trait TestCtor derives CanEqual
  case object Succ extends TestCtor derives CanEqual
  case object Zero extends TestCtor derives CanEqual

  val dummyProof = Proof.Atomic("dummy")

  private val prologSyntax: SimpleProverSyntax[TestPredicate, TestCtor] = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  private final class TestContext(extraAssertions: Seq[Predicate])
      extends TestSmtContext[VariableProvider, Nothing]
      with ProofAssertions[VariableProvider, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    private val commonAssertions = Seq(
      pred(KnownTrue),
      !pred(KnownFalse),
    )

    protected override def assertions: Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[String], Predicate)]] =
      (commonAssertions ++ extraAssertions)
        .map { p =>
          (_: URIO[VariableProvider, TVariable]) => ZIO.succeed(dummyProof -> p)
        } ++
        Seq(
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Or(
              PredicateFunction(Gt, Seq(Variable(n), Variable(m))),
              Or(
                PredicateFunction(Gt, Seq(Variable(m), Variable(n))),
                PredicateFunction(EqualTo, Seq(Variable(n), Variable(m)))
              )
            )
          )
        )


    def assertProves(p: Predicate): ZIO[VariableProvider, Any, TestResult] =
      assertZIO(check(p, fuel))(proven)

    def assertNotProves(p: Predicate): ZIO[VariableProvider, Any, TestResult] =
      assertZIO(check(p, fuel))(notProven)

  }

  private def smtContext(extraAssertions: Predicate*): TestContext = new TestContext(extraAssertions)

  override def spec: Spec[Environment & Scope, Any] =
    suite("Proofs")(
      test("prove true") {
        smtContext().assertProves(pred(KnownTrue))
      },
      test("unproven false") {
        smtContext().assertNotProves(pred(KnownFalse))
      },
      test("prove not false") {
        smtContext().assertProves(!pred(KnownFalse))
      },
      test("prove a given a or b, not b") {
        smtContext(
          pred(Constant("a")) | pred(Constant("b")),
          !pred(Constant("b")),
        ).assertProves(pred(Constant("a")))
      },
      test("prove a given a or b, not (b and c), c") {
        smtContext(
          pred(Constant("a")) | pred(Constant("b")),
          !(pred(Constant("b")) & pred(Constant("c"))),
          pred(Constant("c")),
        ).assertProves(pred(Constant("a")))
      },
      test("unproven a given a or b or c, not (b and c), c") {
        smtContext(
          pred(Constant("a")) | pred(Constant("b")) | pred(Constant("c")),
          !(pred(Constant("b")) & pred(Constant("c"))),
          pred(Constant("c")),
        ).assertNotProves(pred(Constant("a")))
      },
    ).provideSome[Environment](VariableProvider.live)

}
