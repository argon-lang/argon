package dev.argon.prover.smt

import dev.argon.prover.*
import dev.argon.util.Fuel
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

  private final class TestContext(extraAssertions: Seq[Predicate]) extends TestSmtContext[TestPredicate, TestCtor] {
    override val syntax: prologSyntax.type = prologSyntax

    override val fuel = Fuel(100)

    private val commonAssertions = Seq(
      pred(KnownTrue),
      !pred(KnownFalse),
    )

    protected override def freshAssertions: Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[String], Predicate)]] =
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
              pred(Gt, n, m),
              Or(
                pred(Gt, m, n),
                pred(EqualTo, n, m)
              )
            )
          )
        )



  }

  private def smtContext(extraAssertions: Predicate*): TestContext = new TestContext(extraAssertions)

  override def spec: Spec[Environment & Scope, Any] =
    suite("SMT Proofs")(
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
      test("prove x > 0 given not x = 0, not 0 > x") {
        smtContext(
          !pred(Gt, expr(Zero), Expr.Variable("x")),
          !pred(EqualTo, Expr.Variable("x"), expr(Zero)),
        ).assertProves(pred(Gt, Expr.Variable("x"), expr(Zero)))
      },
    ).provideSome[Environment](VariableProvider.live)

}
