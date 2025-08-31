package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*
import zio.test.Assertion.*
import zio.test.*
import cats.*
import cats.implicits.given
import dev.argon.util.Fuel
import zio.interop.catz.core.*

object PrologFuelTests extends ZIOSpecDefault {

  sealed trait TestCtor derives CanEqual
  case object Infinite extends TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestCtor]
  import prologSyntax.*

  private object TestContext extends SimplePrologContext[TestCtor] {

    override val syntax: prologSyntax.type = prologSyntax

    override val fuel: Fuel = Fuel(10)

    override protected def freshAssertions(model: Model): Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[ProofAtom], Predicate)]] =
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
    ).provide(VariableProvider.live)

}
