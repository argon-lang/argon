package dev.argon.prover.prolog

import dev.argon.prover.*
import dev.argon.prover.prolog.SimplePrologContext.VariableProvider
import zio.*
import zio.test.Assertion.*
import zio.test.*

object PrologFuelTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object Infinite extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 10

  private trait TestAssertions[R] extends SimpleProverContext[R, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    protected override val assertions: Seq[URIO[R, TVariable] => URIO[R, (Proof[Unit], Predicate)]] =
      Seq(
        newVariable => (for {
          x <- newVariable
        } yield Proof.Atomic(()) -> (pred(Infinite, v"$x") ==> pred(Infinite, v"$x")))
      )
  }

  private object TestContext extends SimplePrologContext[VariableProvider, Nothing] with TestAssertions[VariableProvider]

  import TestContext.ProofResult

  override def spec: Spec[Environment & Scope, Any] =
    suite("Fuel tests")(
      test("recursive predicate") {
        assertZIO(TestContext.check(pred(Infinite, expr(A)), fuel))(equalTo(ProofResult.Unknown))
      }
    ).provideSome[Environment](VariableProvider.live)

}
