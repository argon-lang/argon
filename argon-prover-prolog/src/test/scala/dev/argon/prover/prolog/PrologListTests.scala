package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import SimplePrologContext.VariableProvider

object PrologListTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object Member extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object Cons extends TestCtor derives CanEqual
  case object Nil extends TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual
  case object B extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  private trait TestAssertions[R] extends SimpleProverContext[R, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    protected override val assertions: Seq[URIO[R, TVariable] => URIO[R, (Proof[Unit], Predicate)]] =
      Seq(
        newVariable => (for {
          x <- newVariable
          t <- newVariable
        } yield Proof.Atomic(()) -> (pred(Member, v"$x", expr(Cons, v"$x", v"$t")))),
        newVariable => (for {
          x <- newVariable
          y <- newVariable
          t <- newVariable
        } yield Proof.Atomic(()) -> (pred(Member, v"$x", v"$t") ==> pred(Member, v"$x", expr(Cons, v"$y", v"$t")))),
      )
  }

  private object FuelContext extends SimplePrologContext[VariableProvider, Nothing] with TestAssertions[VariableProvider]

  private val prologContext = FuelContext
  import prologContext.ProofResult

  override def spec: Spec[Environment & Scope, Any] =
    suite("Lists")(
      test("member with head value (size=1)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Nil))), fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with head value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Cons, expr(B), expr(Nil)))), fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with non-head value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil)))), fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with non-head value (size=3)") {
        assertZIO(prologContext.check(
          pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil))))),
          fuel,
        ))(isSubtype[ProofResult.Yes](anything))
      },
      test("member with missing value (size=1)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Nil))), fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("member with missing value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Nil)))), fuel))(
          equalTo(ProofResult.Unknown)
        )
      },
    ).provideSome[Environment](VariableProvider.live)

}
