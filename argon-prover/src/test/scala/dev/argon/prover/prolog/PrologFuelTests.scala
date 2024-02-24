package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*
import zio.test.Assertion.*
import zio.test.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.*

object PrologFuelTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object Infinite extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  private object TestContext extends SimplePrologContext[TestPredicate, TestCtor] {

    override val syntax: prologSyntax.type = prologSyntax

    override val fuel: Int = 10

    override protected def freshAssertions: Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[ProofAtom], Predicate)]] =
      Seq(
        newVariable => for {
          x <- newVariable
        } yield Proof.Atomic("") -> (pred(Infinite, x) ==> pred(Infinite, x))
      )
  }

  import TestContext.assertNotProves

  override def spec: Spec[Environment & Scope, Any] =
    suite("Prolog Fuel tests")(
      test("recursive predicate") {
        assertNotProves(pred(Infinite, expr(A)))
      },
    ).provideSome[Environment](VariableProvider.live)

}
