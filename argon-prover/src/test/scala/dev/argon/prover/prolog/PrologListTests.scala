package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import cats.*
import cats.implicits.given
import dev.argon.util.Fuel
import zio.interop.catz.core.*

object PrologListTests extends ZIOSpecDefault {

  sealed trait TestCtor derives CanEqual
  case object Member extends TestCtor derives CanEqual
  case object Cons extends TestCtor derives CanEqual
  case object Nil extends TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual
  case object B extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestCtor]
  import prologSyntax.*

  private object FuelContext extends SimplePrologContext[TestCtor] {

    override val syntax: prologSyntax.type = prologSyntax
    
    override val fuel: Fuel = Fuel(100)

    override protected def freshAssertions(model: Model): Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[ProofAtom], Predicate)]] =
      Seq(
        newVariable => for {
          x <- newVariable
          t <- newVariable
        } yield Proof.Atomic("") -> (pred(Member, x, expr(Cons, x, t))),
        newVariable => for {
          x <- newVariable
          y <- newVariable
          t <- newVariable
        } yield Proof.Atomic("") -> (pred(Member, x, t) ==> pred(Member, x, expr(Cons, y, t))),
      )
  }

  private val prologContext = FuelContext
  import prologContext.{ProofResult, fuel}

  override def spec: Spec[Environment & Scope, Any] =
    suite("Prolog Lists")(
      test("member with head value (size=1)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Nil))), Map.empty, fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with head value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Cons, expr(B), expr(Nil)))), Map.empty, fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with non-head value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil)))), Map.empty, fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with non-head value (size=3)") {
        assertZIO(prologContext.check(
          pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil))))),
          Map.empty,
          fuel,
        ))(isSubtype[ProofResult.Yes](anything))
      },
      test("member with missing value (size=1)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Nil))), Map.empty, fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("member with missing value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Nil)))), Map.empty, fuel))(
          equalTo(ProofResult.Unknown)
        )
      },
    ).provide(VariableProvider.live)

}
